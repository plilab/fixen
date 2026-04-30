module Fixen.SymbolSolver.Rule where

import Control.Lens
import Control.Monad
import Data.List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Fixen.Data.NodeId
import Fixen.IR.AST
import Fixen.Monad
import Fixen.SymbolSolver.Common
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
          warnUnusedBoundVars (fst . snd <$> Map.toList unused)
          -- liftIO $ print bvs
          -- actual bound vars is bvs
          -- initialize the rule info
          let lv_info = mkLocalVarInfo bvs
              rul_info =
                RuleInfo
                  { _ruleDeclaration = r
                  , _ruleBoundVars = lv_info
                  }
          -- for now, insert the rule info into the env directly.
          -- TODO: Typecheck
          -- TODO: GetMatchInfo
          -- TODO: Warn name shadowing
          -- TODO: Add free vars to extern
          -- TODO: Ensure no free vars are in assumptions
          return $ env & infoMap . ruleInfoMap . at (getNodeId r) ?~ rul_info
  where
    mkLocalVarInfo mp =
      let mp' = \(v, u) ->
            LocalVarInfo
              { _localVarType = Dynamic
              , _localVarUsage = u
              , _localVarVar = v
              }
       in mp' <$> mp

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
      n = simpleIdentifier <$> (Set.toList $ Set.unions $ getAllExprNames <$> p)
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
