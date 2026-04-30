module Fixen.SymbolSolver.Rule where

import Control.Lens
import Control.Monad

-- import Control.Monad.IO.Class

import Data.List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Fixen.Data.NodeId
import Fixen.IR.AST
import Fixen.Monad
import Fixen.SymbolSolver.Common
import Fixen.SymbolSolver.Extern (initEnvWithExternSymbol)
import Fixen.SymbolSolver.Prelude
import Fixen.SymbolSolver.Validation
import Prelude.Unicode

initEnvWithRule :: SymbolEnv -> Rule -> FixenPass SymbolState SymbolEnv
initEnvWithRule env r = do
  rels_well_formed <- validateRelationsInRule r env
  if not rels_well_formed
    then return env -- ignore this rule
    else do
      -- get the bound variables.
      rule_bound_vars <- getRuleBoundVars r
      -- liftIO $ print rule_bound_vars
      let var_usage_info = Map.unions $ getBoundVarUsageInfo r <$> rule_bound_vars
      any_vars_unbound <- checkUnboundVariable var_usage_info
      if any_vars_unbound
        then return env
        else do
          let (unused, bvs) = Map.partition (\(_, ls) -> null ls) var_usage_info
          warnUnusedBoundVars (fst ∘ snd <$> Map.toList unused)
          -- actual bound vars is bvs
          -- initialize the rule info
          fvs_in_assumptions <- checkFreeVarsInAssumptions r bvs
          if fvs_in_assumptions
            then return env
            else do
              let lv_info = mkLocalVarInfo bvs
                  fvs = getFreeVars r bvs
              -- for now, insert the rule info into the env directly.

              rul_info <-
                RuleInfo
                  { _ruleDeclaration = r
                  , _ruleBoundVars = lv_info
                  }
                  & typeCheck env
              -- TODO: GetMatchInfo
              -- TODO: Warn name shadowing
              env
                & infoMap
                  ∘ ruleInfoMap
                  ∘ at (getNodeId r)
                  ?~ rul_info
                & foldMWith initEnvWithExternSymbol fvs
  where
    mkLocalVarInfo mp =
      let mp' = \(v, u) ->
            LocalVarInfo
              { _localVarType = Dynamic
              , _localVarUsage = u
              , _localVarVar = v
              }
       in mp' <$> mp

typeCheck :: SymbolEnv -> RuleInfo -> FixenPass SymbolState RuleInfo
typeCheck env RuleInfo {_ruleDeclaration = r, _ruleBoundVars = mp} = do
  let type_candidates =
        mp
          <&> _localVarUsage -- get the usage map
          <&> assumptionsOnly -- let's deal with the assumptions first
          <&> catMaybes -- eliminating non assumption uses
          <&> (fmap (mapIndicesToType env r)) -- map each usage to a type
  mp' <- foldListMap (typeCheckType r) mp type_candidates
  -- TODO: type check conclusion

  return $ RuleInfo {_ruleDeclaration = r, _ruleBoundVars = mp'}
  where
    assumptionsOnly = fmap f
    f (UsedInAssumption i j) = Just (i, j)
    f _ = Nothing

foldListMap :: (Monad m, Foldable f) => (a -> k -> b -> m a) -> a -> Map.Map k (f b) -> m a
foldListMap f e m = foldMWithKey f' e m
  where
    f' a k ls = foldM (\a' b' -> f a' k b') a ls

    foldMWithKey :: Monad m => (a -> k -> b -> m a) -> a -> Map.Map k b -> m a
    foldMWithKey f1 e1 m1 = foldM (\a' (k, b) -> f1 a' k b) e1 (Map.toList m1)

typeCheckType :: Rule -> RepresentativeMap LocalVarInfo -> Representative -> (Int, Int, Type) -> FixenPass SymbolState (RepresentativeMap LocalVarInfo)
typeCheckType rule mp v_repr (i, j, t) = do
  let curr_info = mp Map.! v_repr
      curr_type = curr_info ^. localVarType
  case curr_type of
    Bottom -> return mp
    Dynamic ->
      -- update the map
      return $ Map.insert v_repr (curr_info & localVarType .~ ActualType t (TypedViaAssumption i j)) mp
    ActualType t' evidence ->
      if t === t'
        then return mp
        else case evidence of
          TypedViaAssumption i' j' -> do
            let original_asm = rule & ruleAssumptions & (!! i')
                original_var = original_asm & relationParams & (!! j')
                curr_asm = rule & ruleAssumptions & (!! i)
                curr_var = curr_asm & relationParams & (!! j)
            original_var_pos <- fixenGetPosition original_var
            original_ty_pos <- fixenGetPosition t'
            curr_asm_pos <- fixenGetPosition curr_asm
            curr_var_pos <- fixenGetPosition curr_var
            curr_ty_pos <- fixenGetPosition t
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
            return $ Map.insert v_repr (curr_info & localVarType .~ Bottom) mp
          TypedViaConclusion i' -> do
            let original_var = rule & ruleConclusion & relationParams & (!! i')
                curr_asm = rule & ruleAssumptions & (!! i)
                curr_var = curr_asm & relationParams & (!! j)
            original_var_pos <- fixenGetPosition original_var
            original_ty_pos <- fixenGetPosition t'
            curr_asm_pos <- fixenGetPosition curr_asm
            curr_var_pos <- fixenGetPosition curr_var
            curr_ty_pos <- fixenGetPosition t
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
            return $ Map.insert v_repr (curr_info & localVarType .~ Bottom) mp

mapIndicesToType :: SymbolEnv -> Rule -> (Int, Int) -> (Int, Int, Type)
mapIndicesToType env r (i, j) =
  let rel_name =
        ruleAssumptions r !! i
          & relationName
          & simpleIdentifier
   in (env ^. infoMap . relationInfoMap)
        & (Map.! rel_name)
        & (^. relationDeclaration)
        & relationParams
        & (!! j)
        & (i,j,)

getFreeVars :: Rule -> RepresentativeMap (SimpleIdentifier, [UsageInfo]) -> [SimpleIdentifier]
getFreeVars r mp =
  let conds = ruleConditions r <&> conditionExpr <&> getAllExprNames <&> Set.toList & concat
      conc = ruleConclusion r & relationParams <&> getAllExprNames <&> Set.toList & concat
      all_vars = conds ++ conc
   in filter ((`Map.notMember` mp) ∘ simpleIdentifier) all_vars

checkFreeVarsInAssumptions :: Rule -> RepresentativeMap (SimpleIdentifier, [UsageInfo]) -> FixenPass SymbolState Bool
checkFreeVarsInAssumptions r mp = do
  let asms = ruleAssumptions r <&> relationParams & concat & filter (\i -> simpleIdentifier i `Map.notMember` mp)
  if (¬) (null asms)
    then do
      pos <- mapM fixenGetPosition asms
      accumErr
        Nothing
        "free variables in premise"
        ((,This "free variable") <$> pos)
        [ Note "fact-based premises can only contain rule parameters"
        , Hint "add these variables as rule parameters"
        ]
      return True
    else return False

getBoundVarUsageInfo :: Rule -> SimpleIdentifier -> RepresentativeMap (SimpleIdentifier, [UsageInfo])
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
getBoundVarUsageInfoFromAssumption i (idx, Relation _ _ args) =
  zip [0 .. length args - 1] args
    & filter (\(_, a) -> a === i)
    <&> fst
    <&> UsedInAssumption idx

getBoundVarUsageInfoFromCondition :: SimpleIdentifier -> Condition -> [UsageInfo]
getBoundVarUsageInfoFromCondition i c =
  let e = conditionExpr c
      n = simpleIdentifier <$> (Set.toList $ getAllExprNames e)
   in if simpleIdentifier i ∈ n then [UsedInCondition] else []

getBoundVarUsageInfoFromConclusion :: SimpleIdentifier -> Conclusion -> [UsageInfo]
getBoundVarUsageInfoFromConclusion i c =
  let p = relationParams c
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
        case Fixen.IR.AST.ruleBoundVars r of
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
  poss <- mapM fixenGetPosition (Set.toList s)
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
    <&> relationParams
    -- concatenate them to get one giant list of simple identifiers
    & Prelude.concat
    -- get the unique ones
    & Data.List.nubBy (===)

warnUnusedBoundVars :: [SimpleIdentifier] -> FixenPass SymbolState ()
warnUnusedBoundVars ls = do
  when (not (null ls)) $ do
    pos <- mapM fixenGetPosition ls
    accumWarn
      Nothing
      "unused parameter"
      ((,This "variable") <$> pos)
      [Hint "remove these parameters"]

warnBoundVarExtern :: SymbolEnv -> SimpleIdentifier -> FixenPass SymbolState ()
warnBoundVarExtern env i =
  case env ^. infoMap . externInfoMap . at (simpleIdentifier i) of
    Just t -> do
      pos <- fixenGetPosition i
      pos' <- fixenGetPosition t
      accumWarn
        Nothing
        "name shadowing"
        [(pos, This "variable"), (pos', Where "external symbol")]
        [Hint "change the name of this variable"]
    Nothing -> do
      if simpleIdentifier i `Set.member` preludeTerms
        then do
          pos <- fixenGetPosition i
          accumWarn
            Nothing
            "potential name clash with Prelude terms"
            [(pos, This "variable")]
            [Hint "hide this name from the Prelude import, or change the name of this variable"]
        else return ()

validateRelationsInRule :: SymbolValidator Rule
validateRelationsInRule = validate [matchRelationsArity]
  where
    matchRelationsArity :: SymbolRule Rule
    matchRelationsArity r env = do
      let asms = ruleAssumptions r
          concl = ruleConclusion r
      asm_rep <- mapM (\a -> relationExistsAndHasRightArity a "assumption" env) asms
      concl_rep <- relationExistsAndHasRightArity concl "conclusion" env
      return $ concat $ concl_rep : asm_rep

checkUnboundVariable :: RepresentativeMap (SimpleIdentifier, [UsageInfo]) -> FixenPass SymbolState Bool
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
      pos <- fixenGetPosition v
      accumErr
        Nothing
        "unbound rule parameter"
        [(pos, This "variable")]
        [Note "all rule parameters must be bound via some assumption"]
