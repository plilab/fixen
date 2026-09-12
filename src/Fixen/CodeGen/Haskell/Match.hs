{-# LANGUAGE OverloadedStrings #-}

-- | Lower rule forests to Haskell list computations. Index traversal and
-- variable refinement build statements; only Syntax knows Haskell layout.
module Fixen.CodeGen.Haskell.Match (codeGenStep, codeGenStepAll) where

import Data.IntMap.Strict qualified as IntMap
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Fixen.CodeGen.Common
import Fixen.CodeGen.Haskell.Bindings
import Fixen.CodeGen.Haskell.Pattern (matchRulePatterns)
import Fixen.CodeGen.Haskell.Syntax qualified as Hs
import Fixen.CodeGen.RuleInstance (codeGenRuleInstanceName)
import Fixen.IR.AST
import Fixen.IR.RelationRepresentation
import Fixen.IR.RuleForest
import Fixen.Monad

codeGenStep :: CodeGenOptions -> NonEmpty RuleForest -> RelationRepresentation -> FixenPass CodeGenState Text
codeGenStep options forests layouts = do
  body <- case forests of
    forest :| [] -> phaseBody options layouts Nothing forest
    _ -> do
      alternatives <- mapM (\(p, forest) -> (Hs.PCon (numberedName "Phase" p) [],) <$> phaseBody options layouts (Just p) forest) (NonEmpty.zip (0 :| [1 ..]) forests)
      pure $ Hs.Let (Hs.Binding (Hs.pat "db") (Hs.call "selectDb" [Hs.var "i", Hs.var "p"]) :| []) (Hs.Case (Hs.var "p") alternatives)
  let singlePhase = NonEmpty.length forests == 1
      argTypes = if singlePhase then [Hs.typ "Database", Hs.typ "Fact", Hs.typ "Queue"] else [Hs.typ "Interpretation", Hs.typ "Fact", Hs.typ "Phase", Hs.typ "Queue"]
      args = if singlePhase then ["db", "fact", "q"] else ["i", "fact", "p", "q"]
  pure $
    Hs.renderDecls
      [ Hs.Signature (Hs.name "step") (Hs.functionType argTypes (Hs.typ "Queue"))
      , Hs.Function (Hs.name "step") (Hs.pat <$> args) body
      ]

codeGenStepAll :: FixenPass CodeGenState Text
codeGenStepAll = do
  phases <- fixenGetPhases
  let singlePhase = length phases == 1
      args = if singlePhase then ["db", "xs", "q"] else ["i", "xs", "p", "q"]
      argTypes = if singlePhase then [Hs.typ "Database", Hs.TyList (Hs.typ "Fact"), Hs.typ "Queue"] else [Hs.typ "Interpretation", Hs.TyList (Hs.typ "Fact"), Hs.typ "Phase", Hs.typ "Queue"]
      stepArgs = if singlePhase then ["db", "f", "q'"] else ["i", "f", "p", "q'"]
      stepFn = Hs.Lambda (Hs.pat "q'" :| [Hs.pat "f"]) (Hs.call "step" (Hs.var <$> stepArgs))
  pure $
    Hs.renderDecls
      [ Hs.Signature (Hs.name "stepAll") (Hs.functionType argTypes (Hs.typ "Queue"))
      , Hs.Function (Hs.name "stepAll") (Hs.pat <$> args) (Hs.call "foldl'" [stepFn, Hs.var "q", Hs.var "xs"])
      ]

phaseBody :: CodeGenOptions -> RelationRepresentation -> Maybe Int -> RuleForest -> FixenPass CodeGenState Hs.Expr
phaseBody options layouts phase forest = do
  alternatives <- mapM relationCase (Map.toList (_ruleForestTrees forest))
  -- A wildcard is also needed for a phase with no triggered rules.
  let fallback = [(Hs.PWildcard, Hs.var "q") | null alternatives || length alternatives < Map.size layouts]
  pure $ Hs.Case (Hs.var "fact") (NonEmpty.fromList (alternatives ++ fallback))
  where
    relationCase (rel, trees) = do
      let arity = length (_factTypes (_factRepresentation (layouts Map.! rel)))
          incoming = [numberedName "_t" i | i <- [0 .. arity - 1]]
      branches <- mapM (headBranch rel incoming) trees
      pure (Hs.PCon (Hs.name rel) (Hs.PVar <$> incoming), Hs.call "Q.union" [Hs.var "q", Hs.call "Q.fromList" [alternativesExpr (NonEmpty.toList branches)]])
    headBranch rel incoming tree = do
      let fields = _factTypes (_factRepresentation (layouts Map.! rel))
          (headStatements, bindings) = matchHead emptyBindings (zip3 (_ruleTreeChoppedHeadArgs tree) (fst <$> fields) incoming)
          statements = if null incoming then [Hs.guardStmt (Hs.call (dbFactSelector rel) [Hs.var "db"])] else headStatements
      body <- forestBody options layouts phase bindings (_ruleTreeChoppedHeadBranches tree)
      pure (prepend statements body)

alternativesExpr :: [Hs.Expr] -> Hs.Expr
alternativesExpr [] = Hs.List []
alternativesExpr [x] = x
alternativesExpr xs = Hs.call "concat" [Hs.List xs]

prepend :: [Hs.Stmt] -> Hs.Expr -> Hs.Expr
prepend [] body = body
prepend statements (Hs.Do rest result) = Hs.Do (statements ++ rest) result
prepend statements body = Hs.Do statements body

matchHead :: Bindings -> [(Int, QueryType, Hs.Name)] -> ([Hs.Stmt], Bindings)
matchHead bindings [] = ([], bindings)
matchHead bindings ((i, queryType, incoming) : fields) =
  let (statement, next) = matchValue queryType i (Hs.Var incoming) bindings
      (rest, final) = matchHead next fields
   in (statement : rest, final)

-- | A repeated discrete variable is a guard; an ordered variable is refined
-- into a fresh version, leaving earlier bindings available to the expression.
matchValue :: QueryType -> Int -> Hs.Expr -> Bindings -> (Hs.Stmt, Bindings)
matchValue queryType i incoming bindings = case lookupVariable i bindings of
  Nothing -> let (n, next) = bindVariable i bindings in (Hs.LetStmt (Hs.PVar n) incoming, next)
  Just old -> case queryType of
    Match -> (Hs.guardStmt (Hs.call "==" [Hs.Var old, incoming]), bindings)
    Meet {} -> refined Hs.Bind (refinementOperation queryType) old
    LatticeMeet {} -> refined Hs.LetStmt (refinementOperation queryType) old
  where
    refined makeStmt operation old =
      let (n, next) = bindVariable i bindings
       in (makeStmt (Hs.PVar n) (Hs.apps (Hs.Var (lowerName operation)) [Hs.Var old, incoming]), next)

forestBody :: CodeGenOptions -> RelationRepresentation -> Maybe Int -> Bindings -> RuleForest -> FixenPass CodeGenState Hs.Expr
forestBody options layouts phase bindings forest = do
  branches <- mapM branch [(rel, tree) | (rel, trees) <- Map.toList (_ruleForestTrees forest), tree <- NonEmpty.toList trees]
  leaves <- mapM (leafBody options phase bindings) (_ruleForestLeaves forest)
  pure (alternativesExpr (branches ++ leaves))
  where
    branch (rel, tree) = do
      let layout = _databaseRepresentation (layouts Map.! rel)
          fields = zipWith (\position (q, storage, _) -> (_ruleTreeChoppedHeadArgs tree !! position, q, storage)) (IntMap.elems (_extractionMap layout)) (_databaseTypes layout)
          index = Hs.call (dbFactSelector rel) [Hs.var "db"]
          (statements, next) = scanRelation True bindings index fields
      body <- forestBody options layouts phase next (_ruleTreeChoppedHeadBranches tree)
      pure (prepend statements body)

-- | Traverse discrete indexes, then a suffix of ordered fields. Keeping the
-- current index as an expression removes the former sentinel map entry.
scanRelation :: Bool -> Bindings -> Hs.Expr -> [(Int, QueryType, StoreType)] -> ([Hs.Stmt], Bindings)
scanRelation _ bindings index [] = ([Hs.guardStmt index], bindings)
scanRelation _ bindings index ((i, Match, StoredAsHashMap) : fields) =
  let (step, withStep) = freshTemporary bindings
      (statement, next) = case lookupVariable i bindings of
        Nothing ->
          let (n, withVar) = bindVariable i withStep
           in (Hs.Bind (Hs.PTuple [Hs.PVar n, Hs.PVar step]) (Hs.call "HashMap.toList" [index]), withVar)
        Just n -> (Hs.Bind (Hs.PVar step) (Hs.call "maybeToList" [Hs.call "HashMap.lookup" [Hs.Var n, index]]), withStep)
      (rest, final) = scanRelation False next (Hs.Var step) fields
   in (statement : rest, final)
scanRelation _ bindings index [(i, Match, StoredAsHashSet)] = case lookupVariable i bindings of
  Nothing -> let (n, next) = bindVariable i bindings in ([Hs.Bind (Hs.PVar n) (Hs.call "HashSet.toList" [index])], next)
  Just n -> ([Hs.guardStmt (Hs.call "HashSet.member" [Hs.Var n, index])], bindings)
scanRelation root bindings index fields@((_, queryType, _) : _) =
  let (patterns, pending, allocated) = suffixPatterns bindings fields
      -- Fresh temporaries are reserved by suffixPatterns; refinements happen
      -- in field order so duplicate variables in the same tuple work too.
      start = bindings {nextTemporary = nextTemporary allocated}
      (refinements, final) = refineSuffix start pending
      lhs = Hs.tuplePattern patterns
      extract = case queryType of
        Meet {} -> Hs.Bind lhs (Hs.call "HashSet.toList" [index])
        LatticeMeet {} | root -> Hs.Bind lhs (Hs.call "maybeToList" [index])
        LatticeMeet {} -> Hs.LetStmt lhs index
        Match -> error "Fixen.CodeGen: invalid discrete index suffix"
   in (extract : refinements, final)

-- | Unpack the suffix first, then refine repeated variables in field order.
data SuffixBinding
  = IntroduceVariable Int
  | RefineVariable Int QueryType Hs.Name

suffixPatterns :: Bindings -> [(Int, QueryType, StoreType)] -> ([Hs.Pattern], [SuffixBinding], Bindings)
suffixPatterns bindings [] = ([], [], bindings)
suffixPatterns bindings ((i, q, _) : fields) =
  let (n, entry, next) = case lookupVariable i bindings of
        Nothing -> let (v, b) = bindVariable i bindings in (v, IntroduceVariable i, b)
        Just _ -> let (t, b) = freshTemporary bindings in (t, RefineVariable i q t, b)
      (patterns, pending, final) = suffixPatterns next fields
   in (Hs.PVar n : patterns, entry : pending, final)

refineSuffix :: Bindings -> [SuffixBinding] -> ([Hs.Stmt], Bindings)
refineSuffix bindings [] = ([], bindings)
refineSuffix bindings (entry : fields) = case entry of
  IntroduceVariable i -> refineSuffix (snd (bindVariable i bindings)) fields
  RefineVariable i q incoming ->
    let old = boundVariable i bindings
        (n, next) = bindVariable i bindings
        -- Stored values are the first argument here, matching the existing
        -- ordered-index traversal (head matching uses the opposite order).
        statement = case q of
          Meet {} -> Hs.Bind (Hs.PVar n) (Hs.apps (Hs.Var (lowerName (refinementOperation q))) [Hs.Var incoming, Hs.Var old])
          LatticeMeet {} -> Hs.LetStmt (Hs.PVar n) (Hs.apps (Hs.Var (lowerName (refinementOperation q))) [Hs.Var incoming, Hs.Var old])
          Match -> error "Fixen.CodeGen: discrete field after ordered field"
        (rest, final) = refineSuffix next fields
     in (statement : rest, final)

leafBody :: CodeGenOptions -> Maybe Int -> Bindings -> RuleLeaf -> FixenPass CodeGenState Hs.Expr
leafBody options phase bindings leaf = do
  rules <- fixenGetRuleInfo
  let rule = _ruleDeclaration (rules IntMap.! _ruleLeafRuleId leaf)
      ruleName' = codeGenRuleInstanceName rule
      variables = filter ((/= "_") . fst) (sort (zip (_ruleLeafVariableMap leaf) [0 ..]))
      storedNames = Map.fromList [(source, boundVariable i bindings) | (source, i) <- variables]
      (patternStatements, names) = matchRulePatterns (_ruleLeafRuleId leaf) (_ruleLeafPatterns leaf) storedNames
      guards = [Hs.guardStmt (lowerExprWithNames names (conditionExpr condition)) | condition <- _ruleLeafCondition leaf]
      instanceValue = Hs.call ruleName' [Hs.Var (boundVariable i bindings) | (_, i) <- variables]
      instanceBinding = Hs.LetStmt (Hs.pat "rule_instance") instanceValue
      phaseValue = maybe (Hs.var "Nothing") (\p -> Hs.call "Just" [Hs.IntegerLit (toInteger p)]) phase
      debug = [Hs.ExprStmt (Hs.call "debugRuleActivation" [Hs.var "fact", phaseValue, Hs.StringLit ruleName', Hs.var "rule_instance"]) | codeGenDebug options]
      result = maybe (Hs.var "rule_instance") (\p -> Hs.Tuple [Hs.var "rule_instance", Hs.Var (numberedName "Phase" p)]) phase
  pure (Hs.Do (patternStatements ++ guards ++ [instanceBinding] ++ debug) (Hs.call "return" [result]))
