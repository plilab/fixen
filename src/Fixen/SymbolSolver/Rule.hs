{-# LANGUAGE OverloadedStrings #-}

module Fixen.SymbolSolver.Rule (initEnvWithRule) where

import Control.Lens
import Control.Monad
import Data.IntMap.Strict qualified as IntMap
import Data.List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Fixen.Fields
import Fixen.IR.AST
import Fixen.Monad
import Fixen.SymbolSolver.Common
import Fixen.SymbolSolver.Extern (initEnvWithExternSymbol)
import Fixen.SymbolSolver.Validation
import Prelude.Unicode

initEnvWithRule :: SymbolEnv -> Rule -> FixenPass SymbolState SymbolEnv
initEnvWithRule env r = do
  rels_not_well_formed <- validateRelationsInRule r env
  if rels_not_well_formed
    then return env -- ignore this rule
    else do
      -- get the bound variables.
      rule_bound_vars <- getRuleBoundVars r
      let var_usage_info = Map.unions $ getBoundVarUsageInfo r <$> rule_bound_vars
      any_vars_unbound <- checkUnboundVariable var_usage_info
      if any_vars_unbound
        then return env
        else do
          let potentially_shadowed_names = filter (\(_, (_, i)) -> any (not . isUsedInAssumption) i) $ Map.toList var_usage_info
          forM_ potentially_shadowed_names $ \(rep, (i, _)) ->
            validate [warnNameShadowingAgainstExtern "rule parameter" rep] i env
          -- actual bound vars is bvs
          -- initialize the rule info
          fvs_in_assumptions <- checkFreeVarsInAssumptions r var_usage_info
          if fvs_in_assumptions
            then return env
            else do
              let lv_info = mkLocalVarInfo var_usage_info
                  fvs = getFreeVars r var_usage_info
              -- for now, insert the rule info into the env directly.

              rul_info <-
                RuleInfo
                  { _ruleDeclaration = r
                  , _ruleBoundVars = lv_info
                  }
                  & typeCheck env
              env
                & ruleInfos ∘ at (r ^. nodeId) ?~ rul_info
                & foldMWith initEnvWithExternSymbol fvs
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

insertMatchInfo :: Rule -> NameMap (SimpleIdentifier, [UsageInfo]) -> SymbolEnv -> SymbolEnv
insertMatchInfo r mp e = foldl' mkMatched e all_usages
  where
    all_usages =
      mp
        & Map.toList
        <&> snd
        <&> snd
        <&> fmap assumptionsOnly
        <&> catMaybes
        & filter ((> 1) . length)
        & concat
    assumptionsOnly (UsedInAssumption i j) = Just (i, j)
    assumptionsOnly _ = Nothing
    mkMatched :: SymbolEnv -> (Int, Int) -> SymbolEnv
    mkMatched env (i, j) =
      let rel_name =
            r
              & ruleAssumptions
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

typeCheck :: SymbolEnv -> RuleInfo -> FixenPass SymbolState RuleInfo
typeCheck env RuleInfo {_ruleDeclaration = r, _ruleBoundVars = mp} = do
  let asm_type_candidates =
        mp
          <&> _ruleParamUsage -- get the usage map
          <&> assumptionsOnly -- let's deal with the assumptions first
          <&> catMaybes -- eliminating non assumption uses
          <&> (fmap (mapIndicesToType env r)) -- map each usage to a type
      conc_args = r ^. conclusion . args
      conc_type_candidates =
        zip [0 .. length conc_args] conc_args
          & varsOnly
          & catMaybes
          & Map.fromList
          <&> (fmap (mapIndicesToType env r))
      type_candidates = Map.unionWith (++) asm_type_candidates conc_type_candidates
  mp' <- foldListMap (typeCheckType r) mp type_candidates
  return $ RuleInfo {_ruleDeclaration = r, _ruleBoundVars = mp'}
  where
    assumptionsOnly = fmap f
    f (UsedInAssumption i j) = Just $ Left (i, j)
    f _ = Nothing
    varsOnly = fmap g
    g (i, ExprVar _ (IdentifierSimpleIdentifier s))
      | simpleIdentifier s `Map.member` mp = Just $ (simpleIdentifier s, [Right i])
    g (_, _) = Nothing

foldListMap :: (Monad m, Foldable f) => (a -> k -> b -> m a) -> a -> Map.Map k (f b) -> m a
foldListMap f e m = foldMWithKey f' e m
  where
    f' a k ls = foldM (\a' b' -> f a' k b') a ls

    foldMWithKey :: Monad m => (a -> k -> b -> m a) -> a -> Map.Map k b -> m a
    foldMWithKey f1 e1 m1 = foldM (\a' (k, b) -> f1 a' k b) e1 (Map.toList m1)

typeCheckType :: Rule -> NameMap RuleParameterInfo -> Name -> (Either (Int, Int) Int, Type) -> FixenPass SymbolState (NameMap RuleParameterInfo)
typeCheckType r mp v_repr (Left (i, j), t) = do
  let curr_info = mp Map.! v_repr
      curr_type = curr_info ^. ty
  case curr_type of
    Bottom -> return mp
    Dynamic ->
      -- update the map
      return $ Map.insert v_repr (curr_info & ty .~ ActualType t (TypedViaAssumption i j)) mp
    ActualType t' evidence ->
      if t === t'
        then return mp
        else case evidence of
          TypedViaAssumption i' j' -> do
            let original_asm = r & ruleAssumptions & (!! i')
                original_var = original_asm & (^. args) & (!! j')
                curr_asm = r & ruleAssumptions & (!! i)
                curr_var = curr_asm & (^. args) & (!! j)
            original_var_pos <- getPosition original_var
            original_ty_pos <- getPosition t'
            curr_asm_pos <- getPosition curr_asm
            curr_var_pos <- getPosition curr_var
            curr_ty_pos <- getPosition t
            accumErr
              Nothing
              "type mismatch"
              [ (original_var_pos, Where "another occurrence of this variable")
              , (original_ty_pos, Where "has another type")
              , (curr_var_pos, This "this variable")
              , (curr_asm_pos, Where "this assumption")
              , (curr_ty_pos, Where "has this type")
              ]
              [Note "matched variables must have the same type!"]
            return $ Map.insert v_repr (curr_info & ty .~ Bottom) mp
          TypedViaConclusion i' -> do
            let original_var = r ^. conclusion . args & (!! i')
                curr_asm = r ^. assumptions & (!! i)
                curr_var = curr_asm & (^. args) & (!! j)
            original_var_pos <- getPosition original_var
            original_ty_pos <- getPosition t'
            curr_asm_pos <- getPosition curr_asm
            curr_var_pos <- getPosition curr_var
            curr_ty_pos <- getPosition t
            accumErr
              Nothing
              "type mismatch"
              [ (original_var_pos, Where "another occurrence of this variable in the conclusion")
              , (original_ty_pos, Where "has another type")
              , (curr_var_pos, This "this variable")
              , (curr_asm_pos, Where "this assumption")
              , (curr_ty_pos, Where "has this type")
              ]
              [Note "matched variables must have the same type!"]
            return $ Map.insert v_repr (curr_info & ty .~ Bottom) mp
typeCheckType r mp v_repr (Right i, t) = do
  let curr_info = mp Map.! v_repr
      curr_type = curr_info ^. ty
  case curr_type of
    Bottom -> return mp
    Dynamic ->
      -- update the map
      return $ Map.insert v_repr (curr_info & ty .~ ActualType t (TypedViaConclusion i)) mp
    ActualType t' evidence ->
      if t === t'
        then return mp
        else case evidence of
          TypedViaAssumption i' j' -> do
            let original_asm = r ^. assumptions & (!! i')
                original_var = original_asm & (^. args) & (!! j')
                curr_conc = r ^. conclusion
                curr_var = curr_conc & (^. args) & (!! i)
            original_var_pos <- getPosition original_var
            original_ty_pos <- getPosition t'
            curr_var_pos <- getPosition curr_var
            curr_ty_pos <- getPosition t
            accumErr
              Nothing
              "type mismatch"
              [ (original_var_pos, Where "another occurrence of this variable")
              , (original_ty_pos, Where "has another type")
              , (curr_var_pos, This "this variable in the conclusion")
              , (curr_ty_pos, Where "has this type")
              ]
              [Note "matched variables must have the same type!"]
            return $ Map.insert v_repr (curr_info & ty .~ Bottom) mp
          TypedViaConclusion i' -> do
            let original_var = r & ruleConclusion & (^. args) & (!! i')
                curr_conc = r & ruleConclusion
                curr_var = curr_conc & (^. args) & (!! i)
            original_var_pos <- getPosition original_var
            original_ty_pos <- getPosition t'
            curr_var_pos <- getPosition curr_var
            curr_ty_pos <- getPosition t
            accumErr
              Nothing
              "type mismatch"
              [ (original_var_pos, Where "another occurrence of this variable in the conclusion")
              , (original_ty_pos, Where "has another type")
              , (curr_var_pos, This "this variable in the conclusion")
              , (curr_ty_pos, Where "has this type")
              ]
              [Note "matched variables must have the same type!"]
            return $ Map.insert v_repr (curr_info & ty .~ Bottom) mp

mapIndicesToType :: SymbolEnv -> Rule -> Either (Int, Int) Int -> (Either (Int, Int) Int, Type)
mapIndicesToType env r (Left (i, j)) =
  let rel_name =
        ruleAssumptions r !! i
          & (^. name)
          & simpleIdentifier
   in (env ^. relationInfos)
        & (Map.! rel_name)
        & (^. declaration)
        & (^. args)
        & (!! j)
        & (Left (i, j),)
mapIndicesToType env r (Right i) =
  let rel_name =
        ruleConclusion r
          & (^. name)
          & simpleIdentifier
   in (env ^. relationInfos)
        & (Map.! rel_name)
        & (^. declaration)
        & (^. args)
        & (!! i)
        & (Right i,)

getFreeVars :: Rule -> NameMap (SimpleIdentifier, [UsageInfo]) -> [SimpleIdentifier]
getFreeVars r mp =
  let conds = ruleConditions r <&> conditionExpr <&> getAllExprNames <&> Set.toList & concat
      conc = ruleConclusion r & (^. args) <&> getAllExprNames <&> Set.toList & concat
      all_vars = conds ++ conc
   in filter ((/= "_") . simpleIdentifier) $ filter ((`Map.notMember` mp) ∘ simpleIdentifier) all_vars

checkFreeVarsInAssumptions :: Rule -> NameMap (SimpleIdentifier, [UsageInfo]) -> FixenPass SymbolState Bool
checkFreeVarsInAssumptions r mp = do
  let fvs = ruleAssumptions r <&> (^. args) & concat & filter (\i -> simpleIdentifier i /= "_" && simpleIdentifier i `Map.notMember` mp)
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

getBoundVarUsageInfo :: Rule -> SimpleIdentifier -> NameMap (SimpleIdentifier, [UsageInfo])
getBoundVarUsageInfo r v =
  -- walk the assumptions
  let asms = ruleAssumptions r
      idx_asms = zip [0 .. length asms - 1] asms
      cond = ruleConditions r
      conc = ruleConclusion r
      asms_usage =
        idx_asms
          <&> getBoundVarUsageInfoFromAssumption v
          & concat
      cond_usage =
        cond
          <&> getBoundVarUsageInfoFromCondition v
          & concat
      conc_usage = getBoundVarUsageInfoFromConclusion v conc
   in [asms_usage, cond_usage, conc_usage]
        & concat
        & nub
        & (v,)
        & Map.singleton (simpleIdentifier v)

getBoundVarUsageInfoFromAssumption :: SimpleIdentifier -> (Int, Assumption) -> [UsageInfo]
getBoundVarUsageInfoFromAssumption i (idx, Assumption _ _ a) =
  zip [0 .. length a - 1] a
    & filter (\(_, a') -> a' === i)
    <&> fst
    <&> UsedInAssumption idx

getBoundVarUsageInfoFromCondition :: SimpleIdentifier -> Condition -> [UsageInfo]
getBoundVarUsageInfoFromCondition i c =
  let e = conditionExpr c
      n = simpleIdentifier <$> (Set.toList $ getAllExprNames e)
   in if simpleIdentifier i ∈ n then [UsedInCondition] else []

getBoundVarUsageInfoFromConclusion :: SimpleIdentifier -> Conclusion -> [UsageInfo]
getBoundVarUsageInfoFromConclusion i c =
  let p = (^. args) c
      n = p <&> getAllExprNames & Set.unions & Set.toList <&> simpleIdentifier
   in if simpleIdentifier i ∈ n then [UsedInConclusion] else []

-- | Obtains the bound variables of the rule. If they were not defined
-- by the user, they will be inferred from assumptions of the rule. Duplicate
-- bound variables will cause errors. Variables whose name shadows external
-- symbols (including prelude terms) will throw warnings
getRuleBoundVars :: Rule -> FixenPass SymbolState [SimpleIdentifier]
getRuleBoundVars r = do
  -- get the bound variables.
  let rule_bound_vars =
        case r ^. args of
          [] -> getAssumptionVariables r
          v -> v
      -- time to look for duplicate bound variables (that happens when users
      -- specify bound variables and misspelled them probably)
      freq_map =
        rule_bound_vars
          <&> (\i -> Map.singleton (simpleIdentifier i) (Set.singleton i))
          & Map.unionsWith Set.union
      -- filter the values
      dup_vars =
        freq_map
          & Map.filter (\x -> Set.size x > 1)
          & Map.toList
          <&> snd
  -- Get the unique bound variables after throwing errors
  nub_bv <-
    if not (Prelude.null dup_vars)
      then do
        -- handle the errors
        forM_ dup_vars handleDupErrors
        return $ Data.List.nubBy (===) rule_bound_vars
      else return rule_bound_vars
  return nub_bv

handleDupErrors :: Set.Set SimpleIdentifier -> FixenPass SymbolState ()
handleDupErrors s = do
  poss <- mapM getPosition (Set.toList s)
  let errs = (\pos -> (pos, This "variable")) <$> poss
  accumErr
    Nothing
    "duplicate rule parameters"
    errs
    [Note "rule parameters must be unique"]

getAssumptionVariables :: Rule -> [SimpleIdentifier]
getAssumptionVariables r =
  -- get the assumptions
  Fixen.IR.AST.ruleAssumptions r
    -- get the relation parameters
    <&> (^. args)
    -- concatenate them to get one giant list of simple identifiers
    & Prelude.concat
    -- get the unique ones
    & Data.List.nubBy (===)
    -- eliminate holes
    & filter (\i -> simpleIdentifier i /= "_")

validateRelationsInRule :: SymbolValidator Rule
validateRelationsInRule =
  validate
    [ matchRelationsArity
    , checkForRelationsWithAllHoles
    , noDuplicateRuleNames
    ]
  where
    matchRelationsArity :: SymbolRule Rule
    matchRelationsArity r env = do
      let asms = ruleAssumptions r
          concl = ruleConclusion r
      asm_rep <- mapM (\a -> relationExistsAndHasRightArity a "assumption" env) asms
      concl_rep <- relationExistsAndHasRightArity concl "conclusion" env
      return $ concat $ concl_rep : asm_rep
    checkForRelationsWithAllHoles :: SymbolRule Rule
    checkForRelationsWithAllHoles r _ = do
      let asms_all_holes = ruleAssumptions r <&> (^. args) <&> fmap simpleIdentifier & zip (ruleAssumptions r) & filter (\(_, ls) -> all (== "_") ls && length ls > 0) <&> fst
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
    noDuplicateRuleNames :: SymbolRule Rule
    noDuplicateRuleNames r env =
      case ruleName r of
        Nothing -> return []
        Just n -> do
          let conflicting_rule_names =
                env
                  ^. ruleInfos
                  & IntMap.toList
                  <&> snd
                  <&> (^. declaration)
                  <&> ruleName
                  & catMaybes
                  & filter (=== n)
          case conflicting_rule_names of
            [] -> return []
            (x : _) -> do
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

checkUnboundVariable :: NameMap (SimpleIdentifier, [UsageInfo]) -> FixenPass SymbolState Bool
checkUnboundVariable mp = do
  let unbounds = Map.filter isUnbound mp
  unbounds
    & Map.toList
    <&> (\(_, (i, _)) -> i)
    & mapM_ err_on_unbounds
  return $ not $ Map.null unbounds
  where
    isUnbound (_, ls) = usedInConditionOrConclusion ls && not (usedInAssumption ls)
    usedInConditionOrConclusion = any (\x -> x == UsedInCondition || x == UsedInConclusion)
    usedInAssumption =
      any
        ( \x -> case x of
            UsedInAssumption _ _ -> True
            _ -> False
        )
    err_on_unbounds v = do
      pos <- getPosition v
      accumErr
        Nothing
        "unbound rule parameter"
        [(pos, This "variable")]
        [Note "all rule parameters must be bound via some assumption"]
