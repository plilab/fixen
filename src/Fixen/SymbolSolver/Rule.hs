{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Fixen.SymbolSolver.Rule
-- Description : Symbol solving for rules
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module provides facilities for solving and type checking rule
-- declarations.
--
-- @since 26.7
module Fixen.SymbolSolver.Rule where

import Control.Lens
import Control.Monad
import Data.List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Fixen.Fields
import Fixen.IR.AST
import Fixen.Monad
import Fixen.SymbolSolver.Common
import Fixen.SymbolSolver.Extern (initEnvWithExternSymbol)
import Fixen.SymbolSolver.Validation
import Fixen.Utils

--------------------------------------------------------------------------------

-- * Main Entry Point

-- $mainEntryPoint
--
-- You should only need 'initEnvWithRule'.

--------------------------------------------------------------------------------

-- | Initializes a 'SymbolEnv' with a 'Rule'.
--
-- /Precondition/: The 'SymbolEnv' must have been initialized with
-- 'PartialOrdDeclaration's and 'RelationDeclaration's.
--
-- @since 26.7
initEnvWithRule :: SymbolState σ => SymbolEnv -> Rule -> FixenPass σ SymbolEnv
initEnvWithRule env r = do
  rels_not_well_formed <- validateRelationsInRule r env
  if rels_not_well_formed
    then return env -- ignore this rule
    else do
      validateDestructuring env r
      -- get the rule parameters
      rule_parameters <- getRuleParameters r
      -- obtain usage information of the parameters
      let var_usage_info = (⋃) $ getParamUsageInfo r <$> rule_parameters
      -- make sure all parameters are bound to some assumption
      any_vars_unbound <- checkUnboundVariable var_usage_info
      if any_vars_unbound
        then return env
        else do
          -- warn against name shadowing
          let potentially_shadowed_names =
                filter (\(_, (_, i)) -> any ((¬) ∘ isUsedInAssumption) i) $
                  Map.toList var_usage_info
          forM_ potentially_shadowed_names $ \(rep, (i, _)) ->
            validate [warnNameShadowingAgainstExtern "rule parameter" rep] i env
          -- ensure that assumptions do not contain any free variables
          fvs_in_assumptions <- checkFreeVarsInAssumptions r var_usage_info
          if fvs_in_assumptions
            then return env
            else do
              -- initialize the local variable info; vars have no type
              -- information at this point
              let lv_info = mkLocalVarInfo var_usage_info
                  fvs = getFreeVars r var_usage_info
              -- create the base rule info and perform type checking on it to
              -- populate typing information into the rule info
              rul_info <-
                RuleInfo
                  { _ruleDeclaration = r
                  , _ruleBoundVars = lv_info
                  }
                  & typeCheck env
              validateConditionalConclusions env rul_info
              env
                -- insert the rule info
                & ruleInfos . at (r ^. nodeId) ?~ rul_info
                -- insert the free variables as extern symbols
                & foldMWith initEnvWithExternSymbol fvs
                -- insert variable matching info
                <&> insertMatchInfo r var_usage_info
  where
    mkLocalVarInfo mp =
      let mp' = \(v, u) ->
            RuleParameterInfo
              { _ruleParamType = Dynamic
              , _ruleParamUsage = u
              , _ruleParamVar = v
              }
       in mp' <$> mp

--------------------------------------------------------------------------------

-- * Helpers

--------------------------------------------------------------------------------

-- | Performs validation on a rule. The rules are:
--
-- * __Against Relations__: Any relation symbol found in the rule must have
--   an existing relation declaration and must be instantiated with the same
--   arity as declared.
-- * __Assumptions with All Holes__: At the moment, we do not have a use case
--   for including an assumption that contains only holes. There might be a
--   case for "there exists some instantiation of this relation in the DB",
--   but for now, we will treat it as an error until there is some reason not
--   to do so.
-- * __Against Other Rules__: There cannot be duplicate rule names.
-- * __Against Relation Parameters__: A rule name cannot be the same as a
--   named relation parameter.
--
-- @since 26.7
validateRelationsInRule :: SymbolState σ => SymbolValidator σ Rule
validateRelationsInRule =
  validate
    [ matchRelationsArity
    , checkForRelationsWithAllHoles
    , noDuplicateRuleNames
    , noRuleNameMatchingRelationParameter
    ]
  where
    matchRelationsArity r env = do
      let asms = r ^. assumptions
          concl = conclusionFacts (r ^. conclusion)
      -- check the existence and arities of the assumptions and the conclusion
      asm_rep <- mapM (\a -> relationExistsAndHasRightArity a "assumption" env) asms
      concl_rep <- concat <$> mapM (\c -> relationExistsAndHasRightArity c "conclusion" env) concl
      -- return all the errors
      return $ concat $ concl_rep : asm_rep

    checkForRelationsWithAllHoles r _ = do
      let asms_all_holes =
            r
              -- get the assumptions of r
              ^. assumptions
              -- get all the arguments
              ^.. each . args
              -- convert them to identifiers
              <&> fmap (simpleIdentifier . patternStorageVariable)
              -- zip with the original assumptions (we want to throw errors on them)
              & zip (r ^. assumptions)
              -- keep only assumptions with all holes
              & filter (\(_, ls) -> all (== "_") ls ∧ length ls > 0)
              -- remove the argument part, just keep the original assumptions
              <&> fst
      -- throw the errors
      forM asms_all_holes $ \asm -> do
        pos <- getPosition asm
        return $
          Err
            Nothing
            "premise with all holes"
            [(pos, This "premise")]
            [ Note
                "premises cannot have only holes"
            , Hint
                "remove this premise from the rule"
            ]

    noDuplicateRuleNames r env =
      case ruleName r of
        Nothing -> return []
        Just n -> do
          -- get all the rule names that have the same name as r
          let conflicting_rule_names =
                values (env ^. ruleInfos)
                  -- get the declaration names
                  ^.. each . declaration . name
                  -- keep only the rule names that actually have values
                  & catMaybes
                  -- keep only the rule names that are the same as the name of
                  -- this rule
                  & filter (≅ n)
          case conflicting_rule_names of
            [] -> return [] -- all good
            (x : _) -> do
              -- throw the error
              pos <- getPosition r
              pos' <- getPosition x
              return
                [ Err
                    Nothing
                    "duplicate names"
                    [(pos, This "rule"), (pos', Where "another rule with the same name")]
                    [ Note "rules cannot have the same name"
                    , Hint "change the name of one of these rules"
                    ]
                ]

    noRuleNameMatchingRelationParameter r env =
      case ruleName r of
        Nothing -> return []
        Just ruleName' -> do
          let conflictingNames =
                values (env ^. relationInfos)
                  ^.. each . declaration . args . each . name . _Just
                  & filter (≅ ruleName')
          case conflictingNames of
            [] -> return []
            parameterName : _ -> do
              rulePos <- getPosition ruleName'
              parameterPos <- getPosition parameterName
              return
                [ Err
                    Nothing
                    "duplicate names"
                    [ (rulePos, This "rule name")
                    , (parameterPos, Where "relation parameter with the same name")
                    ]
                    [ Note "rule names cannot match relation parameter names"
                    , Hint "rename either the rule or the relation parameter"
                    ]
                ]

-- | Obtains the parameters of the rule. If they were not defined
-- by the user, they will be inferred from assumptions of the rule. Duplicate
-- parameters will cause errors. Parameters whose name shadows external
-- symbols (including prelude terms) will throw warnings
--
-- @since 26.7
getRuleParameters :: SymbolState σ => Rule -> FixenPass σ [SimpleIdentifier]
getRuleParameters r = do
  -- get the parameters.
  let rule_parameters =
        case r ^. args of
          [] -> assumption_variables -- infer from rule
          v -> v ++ (fst <$> ruleDestructuring r)
      -- look for duplicate parameters (that happens when users
      -- specify bound variables and misspelled them probably)
      freq_map =
        rule_parameters
          <&> (\i -> Map.singleton (simpleIdentifier i) (Set.singleton i))
          & Map.unionsWith (∪)
      -- get the duplicate parameters
      dup_vars = values (Map.filter ((> 1) ∘ Set.size) freq_map)
  -- Get the unique parameters after throwing errors
  nub_bv <-
    if (¬) (null dup_vars)
      then do
        -- handle the errors
        forM_ dup_vars handleDupErrors
        return $ Data.List.nubBy (≅) rule_parameters
      else return rule_parameters
  return nub_bv
  where
    -- Throws errors on a set of variables that have duplicates
    handleDupErrors s = do
      poss <- mapM getPosition (Set.toList s)
      let errs = (\pos -> (pos, This "variable")) <$> poss
      accumErr
        Nothing
        "duplicate rule parameters"
        errs
        [Note "rule parameters must be unique"]

    -- the variables in all the assumptions of this rule.
    assumption_variables :: [SimpleIdentifier]
    assumption_variables =
      allAssumptionVariables r
        -- get the unique ones
        & Data.List.nubBy (≅)
        -- eliminate holes
        & filter (\i -> simpleIdentifier i ≠ "_")

-- | Obtains the usage information of a variable within a 'Rule'.
--
-- @since 26.7
getParamUsageInfo
  :: Rule
  -- ^ The 'Rule'
  -> SimpleIdentifier
  -- ^ The variable
  -> NameMap (SimpleIdentifier, [UsageInfo])
getParamUsageInfo r v =
  let asms = r ^. assumptions
      -- index the assumptions in the rule
      idx_asms = zip [0 .. length asms - 1] asms
      cond = r ^. conditions
      conc = r ^. conclusion
      -- get usage info from the assumptions, conditions and conclusion
      asms_usage =
        idx_asms
          <&> getParamUsageInfoFromAssumption v
          & concat
      cond_usage =
        cond
          <&> getParamUsageInfoFromCondition v
          & concat
      conc_usage = [UsedInConclusion | any ((≅ v)) (outerConclusionVariables conc)]
   in -- combine everything together as usage information for this variable
      [asms_usage, cond_usage, conc_usage]
        & concat
        & nub
        & (v,)
        & Map.singleton (simpleIdentifier v)
  where
    getParamUsageInfoFromAssumption
      :: SimpleIdentifier
      -- LHS: the index of the RHS in the rule;
      -- RHS: the assumption in the rule
      -> (Int, Assumption)
      -> [UsageInfo]
    getParamUsageInfoFromAssumption i (idx, Assumption _ _ arg) =
      concat
        [ [UsedInAssumption idx j | patternStorageVariable p ≅ i]
            ++ [UsedInPattern idx j k | isDestructuring p, (k, captured) <- zip [0 ..] (patternVariables p), captured ≅ i]
        | (j, p) <- zip [0 ..] arg
        ]

    getParamUsageInfoFromCondition :: SimpleIdentifier -> Condition -> [UsageInfo]
    getParamUsageInfoFromCondition i c =
      let e = c ^. expr
          n = simpleIdentifier <$> (Set.toList $ getAllExprNames e)
       in if simpleIdentifier i ∈ n then [UsedInCondition] else []

-- | Determines if there are any rule parameters that are not bound by an
-- assumption in the rule. This happens particularly when users explicitly list
-- the rule's parameters.
--
-- @since 26.7
checkUnboundVariable
  :: SymbolState σ
  => NameMap (SimpleIdentifier, [UsageInfo])
  -- ^ The parameter map
  -> FixenPass σ Bool
checkUnboundVariable mp = do
  let unbounds = Map.filter isUnbound mp
  unbounds
    & Map.toList
    <&> (\(_, (i, _)) -> i)
    & mapM_ err_on_unbounds
  return $ (¬) (Map.null unbounds)
  where
    isUnbound (_, ls) = usedInConditionOrConclusion ls ∧ (¬) (usedInAssumption ls)
    usedInConditionOrConclusion = any ((¬) ∘ isUsedInAssumption)
    usedInAssumption = any isUsedInAssumption
    err_on_unbounds v = do
      pos <- getPosition v
      accumErr
        Nothing
        "unbound rule parameter"
        [(pos, This "variable")]
        [Note "all rule parameters must be bound via some assumption"]

-- | Checks if there are any free variables in the assumptions of a rule.
--
-- @since 26.7
checkFreeVarsInAssumptions
  :: SymbolState σ
  => Rule
  -- ^ The 'Rule'
  --
  -- @since 26.7
  -> NameMap (SimpleIdentifier, [UsageInfo])
  -- ^ The variable mappiung
  --
  -- @since 26.7
  -> FixenPass σ Bool
checkFreeVarsInAssumptions r mp = do
  let fvs =
        allAssumptionVariables r
          & filter (\i -> simpleIdentifier i ≠ "_" ∧ simpleIdentifier i ∉ mp)
  if (¬) (null fvs)
    then do
      pos <- mapM getPosition fvs
      accumErr
        Nothing
        "free variables in premise"
        ((,This "free variable") <$> pos)
        [ Note "fact-based premises can only contain rule parameters"
        , Hint "add these variables as rule parameters"
        ]
      return True
    else return False

-- | Both whole stored arguments and captures bind rule parameters. The former
-- have relation-declared types; the latter need not have a known host type.
allAssumptionVariables :: Rule -> [SimpleIdentifier]
allAssumptionVariables r =
  concat
    [ if isDestructuring p then patternStorageVariable p : patternVariables p else [patternStorageVariable p]
    | a <- ruleAssumptions r
    , p <- relationLikeArgs a
    ]

-- | Monotonicity checks deliberately do not inspect host constructors/types.
-- Captures are forbidden in every ordered relation position, even underneath
-- an expression in the conclusion, and regardless of premise ordering.
validateDestructuring :: SymbolState σ => SymbolEnv -> Rule -> FixenPass σ ()
validateDestructuring env r = do
  forM_ (ruleAssumptions r) $ \a ->
    forM_ (zip (relationLikeArgs a) (parameterTypes a)) $ \(p, t) -> when (ordered t) $ do
      when (isDestructuring p) $ do
        pos <- getPosition p
        typePos <- getPosition t
        accumErr
          Nothing
          "cannot destructure a partially ordered/lattice argument"
          [(pos, This "pattern"), (typePos, Where "non-discrete argument type")]
          [Note "destructuring is only allowed in discrete relation positions"]
      rejectCaptures t (patternVariables p)
  forM_ (scopedConclusions (ruleConclusion r)) $ \(bound, c) ->
    forM_ (zip (relationLikeArgs c) (parameterTypes c)) $ \(e, t) ->
      when (ordered t) (rejectCaptures t (filter ((`Set.notMember` bound) . simpleIdentifier) (Set.toList (getAllExprNames e))))
  where
    captures = Map.fromList [(simpleIdentifier v, v) | (_, p) <- ruleDestructuring r, v <- patternVariables p]
    parameterTypes :: RelationLike a -> [Type]
    parameterTypes a = relationParameterType <$> relationLikeArgs ((env ^. relationInfos) Map.! simpleIdentifier (relationLikeName a) ^. declaration)
    ordered t =
      let n = calculateRepresentativeFromType t
       in Map.member n (env ^. partialOrdInfos) || Map.member n (env ^. latticeInfos)
    rejectCaptures t vs = forM_ vs $ \v -> case Map.lookup (simpleIdentifier v) captures of
      Nothing -> pure ()
      Just capture -> do
        pos <- getPosition v
        capturePos <- getPosition capture
        typePos <- getPosition t
        accumErr
          Nothing
          "destructured variable in a partially ordered/lattice position"
          [(pos, This "destructured variable"), (capturePos, Where "captured here"), (typePos, Where "non-discrete argument type")]
          [Note "using a destructured variable in an ordered position may break monotonicity"]

-- | Free identifiers of a conclusion body, excluding lexical case captures.
outerConclusionVariables :: ConclusionBody -> [SimpleIdentifier]
outerConclusionVariables body =
  [v | (bound, e) <- conclusionExpressions body, v <- Set.toList (getAllExprNames e), Set.notMember (simpleIdentifier v) bound]

-- | Case captures have lexical identities, not queued RuleParameterInfo.
-- Check all alternatives (including unreachable ones) without conflating
-- identically spelled captures in sibling or nested alternatives.
validateConditionalConclusions :: SymbolState σ => SymbolEnv -> RuleInfo -> FixenPass σ ()
validateConditionalConclusions env info = do
  let r = _ruleDeclaration info
      caseNames = Set.fromList (simpleIdentifier <$> caseBinders (ruleConclusion r))
      premiseNames = Set.fromList (simpleIdentifier <$> allAssumptionVariables r)
  forM_ (ruleArgs r) $ \v -> when (Set.member (simpleIdentifier v) caseNames && Set.notMember (simpleIdentifier v) premiseNames) $ do
    p <- getPosition v
    accumErr Nothing "case-bound variables cannot be rule parameters" [(p, This "case-local binding")] [Note "case variables are introduced during evaluation, not during premise matching"]
  occurrences <- walk Map.empty (ruleConclusion (_ruleDeclaration info))
  let byBinder = Map.fromListWith (++) [(simpleIdentifierNodeId binder, [(occurrence, t)]) | (binder, occurrence, t) <- occurrences]
  forM_ (Map.elems byBinder) $ \occurrencesForBinder -> case occurrencesForBinder of
    [] -> pure ()
    (first, firstType) : rest -> forM_ rest $ \(other, otherType) -> unless (firstType ≅ otherType) $ do
      p <- getPosition first
      q <- getPosition other
      accumErr Nothing "type mismatch" [(p, Where "another use of this case-bound variable"), (q, This "conflicting relation argument type")] []
  where
    caseBinders (Emit _) = []
    caseBinders (GuardedConclusions arms) = concatMap (caseBinders . snd) arms
    caseBinders (CaseConclusions _ arms) = concatMap (\(p, b) -> patternVariables p ++ caseBinders b) arms
    ordered t =
      let n = calculateRepresentativeFromType t
       in Map.member n (env ^. partialOrdInfos) || Map.member n (env ^. latticeInfos)
    walk locals (Emit cs) = fmap concat $ forM (NonEmpty.toList cs) $ \c -> do
      let argumentTypes = relationParameterType <$> relationLikeArgs ((env ^. relationInfos) Map.! simpleIdentifier (relationLikeName c) ^. declaration)
      fmap concat $ forM (zip (relationLikeArgs c) argumentTypes) $ \(e, t) -> do
        forM_ (Set.toList (getAllExprNames e)) $ \v ->
          forM_ (Map.lookup (simpleIdentifier v) locals) $ \binder -> when (ordered t) $ do
            p <- getPosition v
            q <- getPosition binder
            accumErr Nothing "case-bound variable in a partially ordered/lattice position" [(p, This "case-bound variable"), (q, Where "captured here")] [Note "using a destructured variable in an ordered position may break monotonicity"]
        pure $ case e of
          ExprVar _ (IdentifierSimpleIdentifier v) | Just binder <- Map.lookup (simpleIdentifier v) locals -> [(binder, v, t)]
          _ -> []
    walk locals (GuardedConclusions arms) = concat <$> mapM (walk locals . snd) arms
    walk locals (CaseConclusions e arms) = do
      case e of
        ExprVar _ (IdentifierSimpleIdentifier v)
          | Map.notMember (simpleIdentifier v) locals
          , Just parameter <- Map.lookup (simpleIdentifier v) (_ruleBoundVars info)
          , ActualType t _ <- _ruleParamType parameter
          , ordered t -> do
              p <- getPosition e
              q <- getPosition t
              accumErr Nothing "cannot case-match a partially ordered/lattice value" [(p, This "case scrutinee"), (q, Where "non-discrete type")] [Note "case destructuring is restricted to discrete values"]
        _ -> pure ()
      fmap concat $ forM (NonEmpty.toList arms) $ \(p, b) -> do
        let vs = patternVariables p
            groups = Map.fromListWith (++) [(simpleIdentifier v, [v]) | v <- vs]
        forM_ (filter ((> 1) . length) (Map.elems groups)) $ \duplicates -> do
          positions <- mapM getPosition duplicates
          accumErr Nothing "duplicate case pattern variable" [(pos, This "repeated binder") | pos <- positions] [Note "unlike premise patterns, case patterns must be linear"]
        walk (Map.union (Map.fromList [(simpleIdentifier v, v) | v <- vs]) locals) b

-- | Obtains the free variables of a rule given its parameters.
--
-- @since 26.7
getFreeVars
  :: Rule
  -- ^ The 'Rule'
  -> NameMap (SimpleIdentifier, [UsageInfo])
  -- ^ Information about the rule parameters
  -> [SimpleIdentifier]
getFreeVars r mp =
  let conds =
        r ^. conditions ^.. each . expr
          <&> getAllExprNames
          <&> Set.toList
          & concat
      conc = outerConclusionVariables (r ^. conclusion)
      all_vars = conds ++ conc
   in all_vars
        & filter ((≠ "_") ∘ simpleIdentifier)
        & filter ((∉ mp) ∘ simpleIdentifier)

-- | Performs type checking on a rule.
--
-- @since 26.7
typeCheck
  :: SymbolState σ
  => SymbolEnv
  -- ^ The 'SymbolEnv' used for looking up the type of relation arguments
  --
  -- @since 26.7
  -> RuleInfo
  -- ^ The initial information about the rule (with no type information)
  --
  -- @since 26.7
  -> FixenPass σ RuleInfo
typeCheck env RuleInfo {_ruleDeclaration = the_rule, _ruleBoundVars = param_info} = do
  -- Get everything in the assumptions that need to be type-checked
  let asm_type_candidates =
        param_info
          <&> (^. usageInfo) -- get the usage map
          <&> assumptionsOnly -- let's deal with the assumptions first
          <&> catMaybes -- eliminating non assumption uses
          <&> (fmap (mapIndicesToType env the_rule)) -- map each usage to a type
          -- Get everything in the conclusion that needs to be type-checked
      conc_args =
        [ ((c, j), e)
        | (c, (bound, conc)) <- zip [0 ..] (scopedConclusions (the_rule ^. conclusion))
        , (j, e) <- zip [0 ..] (relationLikeArgs conc)
        , case e of ExprVar _ (IdentifierSimpleIdentifier v) -> Set.notMember (simpleIdentifier v) bound; _ -> True
        ]
      conc_type_candidates =
        conc_args
          & varsOnly -- Get only the arguments to conclusions that are vars
          & catMaybes
          & Map.fromListWith (flip (++))
          <&> (fmap (mapIndicesToType env the_rule))
      type_candidates = Map.unionWith (++) asm_type_candidates conc_type_candidates
  -- Actually perform the type-checking
  param_info' <- foldListMap (typeCheckVar the_rule) param_info type_candidates
  return $ RuleInfo {_ruleDeclaration = the_rule, _ruleBoundVars = param_info'}
  where
    assumptionsOnly = fmap (\case UsedInAssumption i j -> Just $ Left (i, j); _ -> Nothing)
    varsOnly = fmap g
      where
        g (i, ExprVar _ (IdentifierSimpleIdentifier s))
          -- also check that it is actually a rule parameter
          | simpleIdentifier s ∈ param_info = Just $ (simpleIdentifier s, [Right i])
        g (_, _) = Nothing

-- | Maps variable indices to its type with respect to the relevant
-- 'RelationDeclaration'.
--
-- @since 26.7
mapIndicesToType
  :: SymbolEnv
  -- ^ The 'SymbolEnv'
  --
  -- @since 26.7
  -> Rule
  -- ^ The 'Rule'
  --
  -- @since 26.7
  -> Either (Int, Int) (Int, Int)
  -- ^ The index.
  -- @Left (i, j)@ represents the occurrence of a variable in the
  -- \(i^\text{th}\) assumption and \(j^\text{th}\) argument;
  -- @Right (c, j)@ represents the occurrence in argument @j@ of conclusion @c@.
  --
  -- @since 26.7
  -> (Either (Int, Int) (Int, Int), Type)
mapIndicesToType env r (Left (i, j)) =
  let rel_name =
        (r ^. assumptions) !! i
          & (^. name)
          & simpleIdentifier
   in (env ^. relationInfos)
        & (Map.! rel_name)
        & (^. declaration . args)
        & (!! j)
        & (^. ty)
        & (Left (i, j),)
mapIndicesToType env r (Right (c, i)) =
  let rel_name = (NonEmpty.toList (conclusionFacts (r ^. conclusion)) !! c) ^. name & simpleIdentifier
   in (env ^. relationInfos)
        & (Map.! rel_name)
        & (^. declaration . args)
        & (!! i)
        & (^. ty)
        & (Right (c, i),)

-- | Performs type checking on a variable.
--
-- @since 26.7
typeCheckVar
  :: SymbolState σ
  => Rule
  -- ^ The rule being type-checked
  --
  -- @since 26.7
  -> NameMap RuleParameterInfo
  -- ^ Information about the parameters of the rule, including typing information
  --
  -- @since 26.7
  -> Name
  -- ^ The name of the variable
  --
  -- @since 26.7
  -> (Either (Int, Int) (Int, Int), Type)
  -- ^ @(Left (i, j), t)@ represents the occurrence of a variable in the
  -- \(i^\text{th}\) assumption and \(j^\text{th}\) argument;
  -- @(Right (c, j), t)@ represents argument @j@ of conclusion @c@. In both cases, @t@ is its
  -- supposed type.
  --
  -- @since 26.7
  -> FixenPass σ (NameMap RuleParameterInfo)
typeCheckVar the_rule param_info var_name (occ, the_type) = do
  -- Get whatever typing information we have about this variable.
  let curr_info = param_info Map.! var_name
      curr_type = curr_info ^. ty
  case curr_type of
    -- Type-checking for this variable has already failed. Skip.
    Bottom -> return param_info -- don't bother
    -- No type information about this variable is available. Simply insert this
    -- variable, occurrence information and type into the current rule
    -- parameter info. Later, if there is conflicting typing evidence, this
    -- information is used to throw error messages.
    Dynamic ->
      -- create the new type; the typing evidence depends on what occ is.
      let new_ty = case occ of
            Left (i, j) -> ActualType the_type (TypedViaAssumption i j)
            Right (c, j) -> ActualType the_type (TypedViaConclusion c j)
       in -- insert this new information into the current param_info
          return $ Map.insert var_name (curr_info & ty .~ new_ty) param_info
    ActualType t' evidence ->
      if the_type ≅ t'
        then -- types match, all is good. Simply proceed
          return param_info
        else do
          -- type mismatch; throw errors. First, get the position of the
          -- current variable being type-checked
          curr_var_pos <- case occ of
            Left (i, j) -> do
              let thing = (((the_rule ^. assumptions) !! i) ^. args) !! j
              getPosition thing
            Right (c, i) -> do
              let thing = relationLikeArgs (NonEmpty.toList (conclusionFacts (the_rule ^. conclusion)) !! c) !! i
              getPosition thing
          -- we also need the position of the type declaration of this variable
          curr_ty_pos <- getPosition the_type
          -- create the error markers. they are handled differently depending
          -- on the type of the evidence.
          markers <- case evidence of
            TypedViaAssumption i' j' -> do
              let original_asm = the_rule & ruleAssumptions & (!! i')
                  original_var = original_asm & (^. args) & (!! j')
              original_var_pos <- getPosition original_var
              original_ty_pos <- getPosition t'
              return
                [ (original_var_pos, Where "another occurrence of this variable in an assumption")
                , (original_ty_pos, Where "has another type")
                , (curr_var_pos, This "this variable")
                , (curr_ty_pos, Where "has this type")
                ]
            TypedViaConclusion c' i' -> do
              let original_var = relationLikeArgs (NonEmpty.toList (conclusionFacts (the_rule ^. conclusion)) !! c') !! i'
              original_var_pos <- getPosition original_var
              original_ty_pos <- getPosition t'
              return
                [ (original_var_pos, Where "another occurrence of this variable in the conclusion")
                , (original_ty_pos, Where "has another type")
                , (curr_var_pos, This "this variable")
                , (curr_ty_pos, Where "has this type")
                ]

          -- throw the errors
          accumErr Nothing "type mismatch" markers []

          -- let the type be bottom so we can stop type-checking for this
          -- variable
          return $ Map.insert var_name (curr_info & ty .~ Bottom) param_info

-- | Use the 'UsageInfo' to populate match information in relations.
--
-- @since 26.7
insertMatchInfo
  :: Rule
  -- ^ The 'Rule'
  --
  -- @since 26.7
  -> NameMap (SimpleIdentifier, [UsageInfo])
  -- ^ The parameter information
  --
  -- @since 26.7
  -> SymbolEnv
  -- ^ The 'SymbolEnv' to insert the information into
  --
  -- @since 26.7
  -> SymbolEnv
insertMatchInfo r mp e = foldl' mkMatched e all_usages
  where
    all_usages =
      (values mp)
        <&> snd
        <&> fmap assumptionsOnly
        <&> catMaybes
        & filter ((> 1) ∘ length)
        & concat
    assumptionsOnly (UsedInAssumption i j) = Just (i, j)
    assumptionsOnly _ = Nothing
    mkMatched :: SymbolEnv -> (Int, Int) -> SymbolEnv
    mkMatched env (i, j) =
      let rel_name =
            r ^. assumptions
              & (!! i)
              & (^. name)
              & simpleIdentifier
       in env
            & relationInfos
              . ix rel_name
              . matchInfos
              %~ setMatched
      where
        setMatched ls = take j ls ++ (Matched : drop (j + 1) ls)
