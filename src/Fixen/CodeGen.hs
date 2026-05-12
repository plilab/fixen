{-# LANGUAGE OverloadedStrings #-}

module Fixen.CodeGen where

import Data.Char qualified as Char
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Fixen.Fields
import Fixen.IR.AST
import Fixen.IR.RelationRepresentation
import Fixen.IR.RuleForest
import Fixen.Monad
import Fixen.Parser.Token (opChars)

type CodeGenState = SymbolEnv :*: PositionEnv :*: NodeId :*: FixenErrors

codeGen :: NonEmpty RuleForest -> RelationRepresentation -> Program -> FixenPass CodeGenState Text
codeGen forest relation_rep prog = do
  let mod_head = prog ^. moduleName
      std_impts =
        Text.intercalate
          "\n"
          [ "\n----- FIXEN IMPORTS -----"
          , "import Data.HashMap.Strict (HashMap)"
          , "import qualified Data.HashMap.Strict as HashMap"
          , "import Data.HashSet (HashSet)"
          , "import qualified Data.HashSet as HashSet"
          , "import Data.Maybe"
          , "import Control.Monad"
          , "import qualified Data.PQueue.Max as Q"
          ]
      mod_head_code = codeGenModuleDeclaration mod_head
      import_code = if null (prog ^. imports) then "" else Text.append "\n----- USER IMPORTS -----\n" $ Text.intercalate "\n" $ codeGenImportStmt <$> (prog ^. imports)
      hs_blocks_code = codeGenHsBlocks $ prog ^. hsBlocks
      fact_code = codeGenFacts relation_rep
      db_code = codeGenDb relation_rep
      empty_db_code = codeGenEmptyDb relation_rep
  interpretation_code <- codeGenInterpretation
  entailment_code <- codeGenEntailment relation_rep
  insertion_code <- codeGenFactInsertions relation_rep
  cont_code <- codeGenContinuationTypes
  step_code <- codeGenStep forest relation_rep
  loop_and_solve_code <- codeGenLoopAndSolve
  re_solve_code <- codeGenReSolve forest
  q_code <- mapM (codeGenQuery relation_rep) (prog ^. queries)
  return
    $ Text.intercalate
      "\n"
    $ [ mod_head_code
      , std_impts
      , import_code
      , hs_blocks_code
      , "\n----- FACTS -----"
      , fact_code
      , "\n----- FACT DATABASE -----"
      , db_code
      , ""
      , empty_db_code
      ]
      ++ interpretation_code
      ++ [ "\n----- ENTAILMENT -----"
         , entailment_code
         , "\n----- INSERTION -----"
         , insertion_code
         , "\n----- RULE INSTANCES -----"
         , cont_code
         , "\n----- STEP FUNCTION -----"
         , step_code
         , "\n----- SOLVER -----"
         , loop_and_solve_code
         , re_solve_code
         , "\n----- QUERIES -----"
         , Text.intercalate "\n\n" q_code
         ]

codeGenQuery :: RelationRepresentation -> Query -> FixenPass CodeGenState Text
codeGenQuery r q = do
  phases' <- fixenGetPhases
  let num_phases = length phases'
  -- get the type signature
  let fn_name = simpleIdentifier $ queryName q
      rel = queryRel q
      rel_name = simpleIdentifier $ (rel ^. name)
      -- get how the fact is laid out
      rel_rep = r Map.! rel_name
      fact_rep = _factRepresentation rel_rep
      rel_types = _factTypes fact_rep
      modes = rel ^. args
      ty' = zip3 rel_types modes [0 .. length modes - 1]
      a =
        filter
          ( \(_, m, _) -> case m of
              Input _ -> True
              Output _ -> False
          )
          ty'
      ty_sig = (\((_, t), _, _) -> codeGenType t) <$> a
      -- get the vars
      nums = (\(_, _, i) -> i) <$> a
      -- map them via the extraction map
      i_map = _insertionMap fact_rep
      var_nums = (i_map IntMap.!) <$> nums
      var_map = IntMap.fromList $ (,0) <$> var_nums
      vars = (\i -> Text.concat [" _v", Text.show i, "_0"]) <$> var_nums
      body_head = Text.concat [fn_name, Text.concat vars]

  if num_phases == 1
    then do
      remaining <- codeGenQueryStep 0 rel_rep var_map rel_name
      return $
        Text.concat
          [ fn_name
          , " :: "
          , Text.intercalate " -> " ty_sig
          , " -> Database -> [Fact]\n"
          , body_head
          , " db = do\n"
          , remaining
          ]
    else do
      remaining <- codeGenQueryStep 0 rel_rep var_map rel_name
      return $ Text.concat [fn_name, " :: ", Text.intercalate " -> " ty_sig, " -> Interpretation -> Phase -> [Fact]\n", body_head, " i p = do\n  let db = selectDb i p\n", remaining]

codeGenQueryStep :: Int -> RelationRepresentationInfo -> IntMap Int -> Text -> FixenPass CodeGenState Text
codeGenQueryStep n rel_rep name_supply rel_name = do
  let db_rep = _databaseRepresentation rel_rep
  let ty' = _databaseTypes db_rep
  let fact_rep = _factRepresentation rel_rep
  let imap' = _insertionMap fact_rep
  if length ty' == 0
    then return $ Text.concat ["  guard (_fact", rel_name, " db)\n  return ", rel_name]
    else do
      if n == length ty'
        then -- finished, return the fact

          let args' = (\(_, i) -> let num = name_supply IntMap.! i in Text.concat [" _v", Text.show i, "_", Text.show num]) <$> IntMap.toList imap'
           in return $ Text.concat ["  return $ ", rel_name, Text.concat args']
        else case ty' !! n of
          (Match, StoredAsHashMap, _) ->
            -- straightforward match in hashmap
            let prev_step = case name_supply IntMap.!? (-1) of
                  Nothing -> Text.concat ["_facts", rel_name, " ", "db"]
                  Just i -> Text.concat ["step", Text.show i]
                curr_var = n
                new_step = fromMaybe (-1) (name_supply IntMap.!? (-1)) + 1
             in case name_supply IntMap.!? curr_var of
                  Nothing -> do
                    -- no match, just get everything.
                    remaining <- codeGenQueryStep (n + 1) rel_rep (IntMap.insert curr_var 0 (IntMap.insert (-1) new_step name_supply)) rel_name
                    return $ Text.concat ["  (_v", Text.show curr_var, "_0, step", Text.show new_step, ") <- HashMap.toList (", prev_step, ")\n", remaining]
                  Just curr_num -> do
                    -- match.
                    remaining <- codeGenQueryStep (n + 1) rel_rep (IntMap.insert (-1) new_step name_supply) rel_name
                    return $ Text.concat ["  step", Text.show new_step, " <- maybeToList (", prev_step, " HashMap.!? _v", Text.show curr_var, "_", Text.show curr_num, ")\n", remaining]
          (Match, StoredAsHashSet, _) ->
            -- just in the hashset
            -- straightforward match in hashmap
            let prev_step = case name_supply IntMap.!? (-1) of
                  Nothing -> Text.concat ["_facts", rel_name, " ", "db"]
                  Just i -> Text.concat ["step", Text.show i]
                curr_var = n
             in case name_supply IntMap.!? curr_var of
                  Nothing -> do
                    -- no match, just get everything.
                    remaining <- codeGenQueryStep (n + 1) rel_rep (IntMap.insert curr_var 0 name_supply) rel_name
                    return $ Text.concat ["  _v", Text.show curr_var, "_0 <- HashSet.toList (", prev_step, ")\n", remaining]
                  Just curr_num -> do
                    -- match.
                    remaining <- codeGenQueryStep (n + 1) rel_rep name_supply rel_name
                    return $ Text.concat ["  guard (_v", Text.show curr_var, "_", Text.show curr_num, " `HashSet.member` ", prev_step, ")\n", remaining]
          _ -> do
            let prev_step = case name_supply IntMap.!? (-1) of
                  Nothing -> Text.concat ["_facts", rel_name, " ", "db"]
                  Just i -> Text.concat ["step", Text.show i]
                lhs' =
                  ( \i -> case name_supply IntMap.!? i of
                      Nothing -> Text.concat ["_v", Text.show i, "_0"]
                      Just num -> Text.concat ["_v", Text.show i, "_", Text.show (num + 1)]
                  )
                    <$> [n .. length ty' - 1]
                lhs_tup = if length lhs' == 1 then lhs' !! 0 else Text.concat ["(", Text.intercalate ", " lhs', ")"]
                fst_i = Text.concat ["  ", lhs_tup, " <- HashSet.toList ", prev_step, "\n"]
                need_to_leq = filter (`IntMap.member` name_supply) [n .. length ty' - 1]
                guards = (\i -> Text.concat ["  guard (", codeGenIdentifier $ get_leq ty' i, " _v", Text.show i, "_0 _v", Text.show i, "_1)\n"]) <$> need_to_leq
            remaining <- codeGenQueryStep (n + 1) rel_rep (IntMap.unionsWith max [name_supply, IntMap.fromList [(i, 0) | i <- [n .. length ty' - 1]], IntMap.fromList [(i, 1) | i <- need_to_leq]]) rel_name
            return $ Text.concat [fst_i, Text.concat guards, remaining]
  where
    get_leq ty' i =
      case ty' !! i of
        (Meet l _, _, _) -> l
        _ -> error "panic: discrete term found after partially ordered terms!"

codeGenReSolve :: NonEmpty RuleForest -> FixenPass CodeGenState Text
codeGenReSolve f =
  case f of
    x :| [] -> do
      let leaves = _ruleForestLeaves x
      l_c <- mapM (codeGenFactLeaves Nothing) leaves
      return $
        Text.concat
          [ "\nreSolve :: Database -> [Fact] -> Database\nreSolve db f =\n  let q = Q.fromList $ concat [\n            Init <$> f"
          , Text.concat (Text.cons ',' <$> l_c)
          , "\n          ]\n   in loop q db"
          ]
    _ -> do
      let numbered = (NonEmpty.toList $ NonEmpty.zip (1 :| [2 .. length f]) f) >>= (\(i, ls) -> (i,) <$> _ruleForestLeaves ls)
      l_c <- mapM (\(i, f') -> codeGenFactLeaves (Just i) f') numbered
      return $
        Text.concat
          [ "\nreSolve :: Interpretation -> [Fact] -> Interpretation\nreSolve i f =\n  let q = Q.fromList $ concat [\n            (,Phase"
          , Text.show (length f)
          , ") . Init <$> f"
          , Text.concat (Text.cons ',' <$> l_c)
          , "\n          ]\n   in loop q i"
          ]

codeGenFactLeaves :: Maybe Int -> RuleLeaf -> FixenPass CodeGenState Text
codeGenFactLeaves phase_no leaf = do
  all_phases <- fixenGetPhases
  let num_phases = length all_phases
      conds = _ruleLeafCondition leaf
      conds_code =
        ( \x ->
            Text.concat
              [ stepIndent 1
              , "guard ("
              , codeGenExprWithNameReplacement IntMap.empty Map.empty (conditionExpr x)
              , ")"
              ]
        )
          <$> conds
      conc = _ruleLeafConclusion leaf
      conc_code = case phase_no of
        Nothing ->
          Text.concat
            [ stepIndent 1
            , "[Init ("
            , simpleIdentifier $ conc ^. name
            , " "
            , Text.intercalate " " $ codeGenExprWithNameReplacement IntMap.empty Map.empty <$> (conc ^. args)
            , ")]"
            ]
        Just phase ->
          Text.concat
            [ stepIndent 1
            , "[(Init ("
            , simpleIdentifier $ (conc ^. name)
            , " "
            , Text.intercalate " " $ codeGenExprWithNameReplacement IntMap.empty Map.empty <$> (conc ^. args)
            , "), Phase"
            , Text.show (mod phase num_phases + 1)
            , ")]"
            ]
  return $ Text.append (Text.concat conds_code) conc_code

codeGenLoopAndSolve :: FixenPass CodeGenState Text
codeGenLoopAndSolve = do
  p <- fixenGetPhases
  if length p == 1
    then return "loop :: Queue -> Database -> Database\nloop q db\n  | Just (p, q') <- Q.maxView q =\n    let f = evaluate p\n     in case insertToDb db f of\n          Nothing -> loop q' db\n          Just db' -> loop (step db' f q') db'\n  | otherwise = db\n\nsolve :: [Fact] -> Database\nsolve = reSolve emptyDb"
    else return "loop :: Queue -> Interpretation -> Interpretation\nloop q i\n  | Just (p, q') <- Q.maxView q =\n    let (f, phase) = evaluatePhased p\n     in case insertToInterpretation i f phase of\n          Nothing -> loop q' i\n          Just i' -> loop (step i' f phase q') i'\n  | otherwise = i\n\nsolve :: [Fact] -> Interpretation\nsolve = reSolve emptyInterpretation"

codeGenStep :: NonEmpty RuleForest -> RelationRepresentation -> FixenPass CodeGenState Text
codeGenStep f r = do
  case f of
    x :| [] -> do
      phase_code <- codeGenStepSinglePhase x r Nothing
      return $ Text.concat ["step :: Database -> Fact -> Queue -> Queue \nstep db fact q = case fact of\n", phase_code]
    _ -> do
      c <- codeGenStepMultiPhase f r
      return $ Text.concat ["step :: Interpretation -> Fact -> Phase -> Queue -> Queue\nstep i f p q = let db = selectDb i p in case p of", c]

codeGenStepMultiPhase :: NonEmpty RuleForest -> RelationRepresentation -> FixenPass CodeGenState Text
codeGenStepMultiPhase xs r = do
  let zipped = NonEmpty.zip (1 :| [2 .. length xs]) xs
  ls <- mapM (codeGenStepMultiPhaseCase r) zipped
  return $ Text.concat $ NonEmpty.toList ls

codeGenStepMultiPhaseCase :: RelationRepresentation -> (Int, RuleForest) -> FixenPass CodeGenState Text
codeGenStepMultiPhaseCase r (no, f) = do
  let header = Text.concat ["\n  Phase", Text.show no, " -> case f of\n"]
  res <- codeGenStepSinglePhase f r (Just no)
  return $ Text.concat [header, res]

codeGenStepSinglePhase :: RuleForest -> RelationRepresentation -> Maybe Int -> FixenPass CodeGenState Text
codeGenStepSinglePhase f r phase_no = do
  let cases = Map.toList $ _ruleForestTrees f
  cases_code <- mapM (codeGenStepSinglePhaseCase r phase_no) cases
  rels <- fixenGetRelationInfo
  if length rels == length cases
    then return $ Text.intercalate "\n" cases_code
    else return $ Text.concat [Text.intercalate "\n" cases_code, "\n    _ -> q"]

codeGenStepSinglePhaseCase :: RelationRepresentation -> Maybe Int -> (Text, NonEmpty RuleTreeChoppedHead) -> FixenPass CodeGenState Text
codeGenStepSinglePhaseCase r phase_no (rel_name, br) = do
  r_info_map <- fixenGetRelationInfo
  let r_info = r_info_map Map.! rel_name
      arity = length $ _relationArgMatchInfo r_info
      case_vars = Text.append "_t" <$> Text.show <$> [0 .. arity - 1]
      header = Text.concat ["    ", rel_name, " ", Text.intercalate " " case_vars, " -> Q.union q $ Q.fromList $ "]
  branches <- mapM (codeGenSinglePhaseCaseStart r rel_name 0 IntMap.empty 0 phase_no) br
  if length branches > 1
    then return $ Text.concat [header, "concat [\n", Text.intercalate ",\n" $ NonEmpty.toList branches, "\n      ]"]
    else return $ Text.concat [header, "\n", Text.concat $ NonEmpty.toList branches]

codeGenSinglePhaseCaseStart :: RelationRepresentation -> Text -> Int -> IntMap Int -> Int -> Maybe Int -> RuleTreeChoppedHead -> FixenPass CodeGenState Text
codeGenSinglePhaseCaseStart rel_rep rel_name curr_pos name_supply indent phase_no tree = do
  -- curr_pos is the t something something. To get the v something something, look at
  -- tree_args !! curr_pos
  let tree_args = _ruleTreeChoppedHeadArgs tree
  if length tree_args == 0
    then do
      remaining <- codeGenSinglePhaseForest rel_rep name_supply indent (_ruleTreeChoppedHeadBranches tree) phase_no
      return $ Text.concat ["      do\n        guard (_facts", rel_name, " db)", remaining]
    else
      if curr_pos < length tree_args
        then do
          let curr_v = tree_args !! curr_pos
          -- does the name exist in the name supply? otherwise, just do the trivial thing
          if curr_v `IntMap.notMember` name_supply
            then
              if curr_pos == 0
                then do
                  remaining <-
                    codeGenSinglePhaseCaseStart
                      rel_rep
                      rel_name
                      (curr_pos + 1)
                      (IntMap.insert 0 0 name_supply)
                      indent
                      phase_no
                      tree
                  return $ Text.concat ["      do\n        let _v0_0 = _t0", remaining]
                else do
                  -- no leading do
                  remaining <-
                    codeGenSinglePhaseCaseStart
                      rel_rep
                      rel_name
                      (curr_pos + 1)
                      (IntMap.insert curr_v 0 name_supply)
                      indent
                      phase_no
                      tree
                  let header = Text.concat ["\n        let _v", Text.show curr_v, "_0 = _t", Text.show curr_pos]
                  return $ Text.concat [header, remaining]
            else do
              -- do the match with the existing var.
              let curr_var_curr_number = name_supply IntMap.! curr_v
                  -- match _v(curr_v)_(curr_v_curr_number) against _t(curr_pos). Check
                  -- how to match
                  (q_ty, _) = (_factTypes $ _factRepresentation (rel_rep Map.! rel_name)) !! curr_pos
              -- rec call
              remainin <-
                codeGenSinglePhaseCaseStart rel_rep rel_name (curr_pos + 1) (IntMap.insert curr_v (curr_var_curr_number + 1) name_supply) indent phase_no tree
              -- match depending on the kind of matching alg.
              case q_ty of
                Match -> do
                  remaining <-
                    codeGenSinglePhaseCaseStart rel_rep rel_name (curr_pos + 1) name_supply indent phase_no tree
                  return $
                    Text.concat
                      [ "\n        guard (_v"
                      , Text.show curr_v
                      , "_"
                      , Text.show curr_var_curr_number
                      , " == _t"
                      , Text.show curr_pos
                      , ")"
                      , remaining
                      ]
                Meet _ mlbs_fn ->
                  return $
                    Text.concat
                      [ "\n        _v"
                      , Text.show curr_v
                      , "_"
                      , Text.show (curr_var_curr_number + 1)
                      , " <- "
                      , codeGenIdentifier mlbs_fn
                      , " _v"
                      , Text.show curr_v
                      , "_"
                      , Text.show curr_var_curr_number
                      , " _t"
                      , Text.show curr_pos
                      , remainin
                      ]
        else -- proceed with the next calls.
          codeGenSinglePhaseForest rel_rep name_supply indent (_ruleTreeChoppedHeadBranches tree) phase_no

codeGenSinglePhaseForest :: RelationRepresentation -> IntMap Int -> Int -> RuleForest -> Maybe Int -> FixenPass CodeGenState Text
codeGenSinglePhaseForest rel_rep name_supply indent forest phase_no = do
  let branches = (Map.toList $ _ruleForestTrees forest) >>= (\(t, l) -> (t,) <$> NonEmpty.toList l)
      leaves = _ruleForestLeaves forest
  if length branches + length leaves <= 1
    then -- continue in the straight line

      if length branches == 0
        then do
          leaves_code <- mapM (codeGenSinglePhaseLeaves name_supply indent phase_no) leaves
          return $ Text.concat leaves_code
        else do
          branches_code <- mapM (codeGenSinglePhaseBranch rel_rep name_supply indent 0 phase_no) branches
          return $ Text.concat branches_code
    else do
      leaves_code <- mapM (codeGenSinglePhaseLeaves name_supply (indent + 1) phase_no) leaves
      branches_code <- mapM (codeGenSinglePhaseBranch rel_rep name_supply (indent + 1) 0 phase_no) branches
      -- branch out, indent by 1. put do headers everywhere
      return $ Text.concat [stepIndent indent, "concat [", Text.intercalate "," (Text.append (Text.concat [stepIndent indent, "  do"]) <$> branches_code ++ leaves_code), stepIndent indent, "  ]"]

codeGenSinglePhaseLeaves :: IntMap Int -> Int -> Maybe Int -> RuleLeaf -> FixenPass CodeGenState Text
codeGenSinglePhaseLeaves name_supply indent phase_no leaf = do
  let rule_id = _ruleLeafRuleId leaf
      rule_cond = _ruleLeafCondition leaf
      -- rule_conc = _ruleLeafConclusion leaf
      rule_v_map = _ruleLeafVariableMap leaf
      -- map indices to indices. Currently, it's in the form of [a, c, b], etc.
      -- sort by name, preserving original positions
      m1 = filter (\(x, _) -> x /= "_") $ sort (zip rule_v_map [0 .. length rule_v_map - 1])
      expr_map = Map.fromList m1
  -- flipping m1 and keeping only the Ints gives u which v_? to put into the rule instance
  name' <- codeGenGetRuleContinuationName rule_id
  let cond_exprs = conditionExpr <$> rule_cond
      cond_code = Text.concat $ (\t -> Text.concat [stepIndent indent, "guard (", t, ")"]) <$> codeGenExprWithNameReplacement name_supply expr_map <$> cond_exprs
      cont_comp = (\(_, i) -> Text.concat ["_v", Text.show i, "_", Text.show (name_supply IntMap.! i)]) <$> m1
      cont_code = Text.concat $ Text.append " " <$> cont_comp
  case phase_no of
    Nothing ->
      return $
        Text.concat
          [ cond_code
          , stepIndent indent
          , "return $ "
          , name'
          , cont_code
          ]
    Just p ->
      return $
        Text.concat
          [ cond_code
          , stepIndent indent
          , "return ("
          , name'
          , cont_code
          , ", Phase"
          , Text.show p
          , ")"
          ]

codeGenExprWithNameReplacement :: IntMap Int -> Map Text Int -> Expr -> Text
codeGenExprWithNameReplacement name_supply var_map (ExprVar _ (IdentifierSimpleIdentifier (SimpleIdentifier _ i)))
  | i `Map.member` var_map =
      let c_v = var_map Map.! i
       in Text.concat ["_v", Text.show c_v, "_", Text.show $ name_supply IntMap.! c_v]
codeGenExprWithNameReplacement _ _ (ExprVar _ i) = codeGenIdentifier i
codeGenExprWithNameReplacement name_supply var_map (ExprApp _ lhs' rhs') =
  Text.concat ["(", codeGenExprWithNameReplacement name_supply var_map lhs', " ", codeGenExprWithNameReplacement name_supply var_map rhs', ")"]
codeGenExprWithNameReplacement _ _ (ExprIntLit _ i) = Text.show i
codeGenExprWithNameReplacement _ _ (ExprStrLit _ s) = Text.show s
codeGenExprWithNameReplacement name_supply var_map (ExprTuple _ hd tl) =
  let hd' = codeGenExprWithNameReplacement name_supply var_map hd
      tl' = codeGenExprWithNameReplacement name_supply var_map <$> NonEmpty.toList tl
      comp = Text.intercalate ", " $ hd' : tl'
   in Text.concat ["(", comp, ")"]
codeGenExprWithNameReplacement name_supply var_map (ExprList _ ls) =
  let comp = codeGenExprWithNameReplacement name_supply var_map <$> ls
      comp_code = Text.intercalate ", " comp
   in Text.concat ["[", comp_code, "]"]
codeGenExprWithNameReplacement _ _ (ExprUnit _) = "()"

codeGenSinglePhaseBranch :: RelationRepresentation -> IntMap Int -> Int -> Int -> Maybe Int -> (Text, RuleTreeChoppedHead) -> FixenPass CodeGenState Text
codeGenSinglePhaseBranch rel_rep name_supply indent curr_pos phase_no (rel_name, tree) = do
  let tree_args = _ruleTreeChoppedHeadArgs tree
  if length tree_args == 0
    then do
      remaining <- codeGenSinglePhaseForest rel_rep name_supply indent (_ruleTreeChoppedHeadBranches tree) phase_no
      return $ Text.concat [stepIndent indent, "guard (_facts", rel_name, " db)", remaining]
    else
      if curr_pos < length tree_args
        then do
          let -- curr_pos is not actually what we want. We should start querying from
              -- the first thing in the database, which can be found by
              -- taking the extractionMap Map.! curr_pos
              fact_rep = _databaseRepresentation (rel_rep Map.! rel_name)
              i_map = _extractionMap fact_rep
              real_curr_pos = i_map IntMap.! curr_pos
              curr_v = tree_args !! real_curr_pos
              -- also get whether we are dealing with a hashmap or a hashset.
              db_ty = _databaseTypes fact_rep
          case db_ty !! curr_pos of -- use curr_pos because curr_pos corresponds to the step number, and we are actually walking down the DB representation
            (_, StoredAsHashMap, _) -> do
              -- is a hashmap.
              case name_supply IntMap.!? curr_v of
                Nothing ->
                  -- no match.
                  -- check curr_pos. If it is equal to 0, then need to pull _facts... from db.
                  if curr_pos == 0
                    then do
                      -- (_v(curr_v)_0, stepN) <- HashMap.toList (_facts... db)
                      -- get the N ^
                      let new_stepNno = (fromMaybe (-1) (name_supply IntMap.!? (-1))) + 1
                      remaining <- codeGenSinglePhaseBranch rel_rep (IntMap.insert curr_v 0 $ IntMap.insert (-1) new_stepNno name_supply) indent (curr_pos + 1) phase_no (rel_name, tree)
                      return $
                        Text.concat
                          [ stepIndent indent
                          , "(_v"
                          , Text.show curr_v
                          , "_0, step"
                          , Text.show new_stepNno
                          , ") <- HashMap.toList (_facts"
                          , rel_name
                          , " db)"
                          , remaining
                          ]
                    else do
                      -- must have been a previous step number here since curr_pos > 0. Otherwise,
                      -- what was the previous step(s) doing?
                      let old_step_no = name_supply IntMap.! (-1)
                          new_step_no = old_step_no + 1
                      remaining <-
                        codeGenSinglePhaseBranch
                          rel_rep
                          (IntMap.insert curr_v 0 $ IntMap.insert (-1) new_step_no name_supply)
                          indent
                          (curr_pos + 1)
                          phase_no
                          (rel_name, tree)
                      return $
                        Text.concat
                          [ stepIndent indent
                          , "(_v"
                          , Text.show curr_v
                          , "_0, step"
                          , Text.show new_step_no
                          , ") <- HashMap.toList step"
                          , Text.show old_step_no
                          , remaining
                          ]
                Just curr_v_curr_number -> do
                  -- match.
                  let new_step_no = fromMaybe (-1) (name_supply IntMap.!? (-1)) + 1
                  remaining <- codeGenSinglePhaseBranch rel_rep (IntMap.insert (-1) new_step_no name_supply) indent (curr_pos + 1) phase_no (rel_name, tree)
                  if curr_pos == 0
                    then do
                      return $
                        Text.concat
                          [ stepIndent indent
                          , "step"
                          , Text.show new_step_no
                          , " <- maybeToList (_facts"
                          , rel_name
                          , " db HashMap.!? _v"
                          , Text.show curr_v
                          , "_"
                          , Text.show curr_v_curr_number
                          , ")"
                          , remaining
                          ]
                    else do
                      return $
                        Text.concat
                          [ stepIndent indent
                          , "step"
                          , Text.show new_step_no
                          , " <- maybeToList (step"
                          , Text.show (new_step_no - 1)
                          , " HashMap.!? _v"
                          , Text.show curr_v
                          , "_"
                          , Text.show curr_v_curr_number
                          , ")"
                          , remaining
                          ]
            (Match, StoredAsHashSet, _) -> do
              let prev_step = if curr_pos == 0 then Text.concat ["(_facts", rel_name, " db)"] else Text.concat ["step", Text.show (name_supply IntMap.! (-1))]
              -- discrete, but last guy. Just check for element membership
              case name_supply IntMap.!? curr_v of
                Nothing -> do
                  -- no match, instantiate new var, i.e., v_.. <- HashSet.toList step...
                  remaining <- codeGenSinglePhaseBranch rel_rep (IntMap.insert curr_v 0 name_supply) indent (curr_pos + 1) phase_no (rel_name, tree)
                  return $
                    Text.concat
                      [ stepIndent indent
                      , "_v"
                      , Text.show curr_v
                      , "_0 <- HashSet.toList "
                      , prev_step
                      , remaining
                      ]
                Just curr_v_curr_number -> do
                  -- just check if curr_v is in the previous step
                  remaining <- codeGenSinglePhaseBranch rel_rep name_supply indent (curr_pos + 1) phase_no (rel_name, tree)
                  return $
                    Text.concat
                      [ stepIndent indent
                      , "guard (_v"
                      , Text.show curr_v
                      , "_"
                      , Text.show curr_v_curr_number
                      , " `HashSet.member` "
                      , prev_step
                      , ")"
                      , remaining
                      ]
            (Meet _ _, _, _) -> do
              -- get the list of supposed var names.
              -- fake curr_pos
              let prev_step = if curr_pos == 0 then Text.concat ["(_facts", rel_name, " db)"] else Text.concat ["step", Text.show (name_supply IntMap.! (-1))]
              let fake_curr_pos = [curr_pos .. length tree_args - 1]
                  real_curr_poses = (i_map IntMap.!) <$> fake_curr_pos
                  list_curr_v = (tree_args !!) <$> real_curr_poses
                  folder :: Int -> [Int] -> [Either Int (Int, Int)]
                  folder _ [] = []
                  folder ctr (c_v : xs) = if c_v `IntMap.notMember` name_supply then Left c_v : folder ctr xs else Right (c_v, ctr) : folder (ctr + 1) xs
                  lhs' = folder (fromMaybe (-1) (name_supply IntMap.!? (-1)) + 1) list_curr_v
                  lhs_tup_comp =
                    ( \t -> case t of
                        Right (_, i) -> Text.append "step" (Text.show i)
                        Left i -> Text.concat ["_v", Text.show i, "_0"]
                    )
                      <$> lhs'
                  lhs_tup =
                    if length lhs_tup_comp == 1
                      then lhs_tup_comp !! 0
                      else Text.concat ["(", Text.intercalate ", " lhs_tup_comp, ")"]
                  remaining_steps n_s [] = (n_s, [])
                  remaining_steps n_s (Left c_v : xs) = remaining_steps (IntMap.insert c_v 0 n_s) xs
                  remaining_steps n_s (Right (c_v, str) : xs) =
                    let n_s' = IntMap.insert (-1) str $ IntMap.insert c_v ((n_s IntMap.! c_v) + 1) n_s
                        (q_ty, _, _) = db_ty !! (length db_ty - length xs - 1)
                        q_id = case q_ty of
                          Meet _ l -> l
                          _ -> error "panic! Discrete variable coming after partially ordered variable"
                        hd =
                          Text.concat
                            [ stepIndent indent
                            , "_v"
                            , Text.show c_v
                            , "_"
                            , Text.show (n_s' IntMap.! c_v)
                            , " <- "
                            , codeGenIdentifier q_id
                            , " step"
                            , Text.show str
                            , " _v"
                            , Text.show c_v
                            , "_"
                            , Text.show (n_s IntMap.! c_v)
                            ]
                        (n_s'', ls) = remaining_steps n_s' xs
                     in (n_s'', hd : ls)
                  (name_supply', r_s) = remaining_steps name_supply lhs'
              remaining <- codeGenSinglePhaseForest rel_rep name_supply' indent (_ruleTreeChoppedHeadBranches tree) phase_no
              -- is a hashset. walk down set and mlbs
              return $
                Text.concat
                  [ stepIndent indent
                  , lhs_tup
                  , " <- HashSet.toList "
                  , prev_step
                  , Text.concat r_s
                  , remaining
                  ]
        else -- proceed with the next calls.
          codeGenSinglePhaseForest rel_rep name_supply indent (_ruleTreeChoppedHeadBranches tree) phase_no

stepIndent :: Int -> Text
stepIndent 0 = Text.cons '\n' $ Text.replicate 8 " "
stepIndent n = Text.cons '\n' $ Text.replicate (8 + (4 * n)) " "

-- else return "?"

codeGenContinuationTypes :: FixenPass CodeGenState Text
codeGenContinuationTypes = do
  rule_map <- fixenGetRuleInfo
  let rules' = IntMap.toList rule_map
  rule_cases <- concat <$> mapM codeGenRuleContinuationType rules'
  phase_info <- fixenGetPhases
  let q_ty = if length phase_info == 1 then "RuleInstance" else "(RuleInstance, Phase)"
      eval_decl = "\n\nevaluate :: RuleInstance -> Fact\n"
  eval_cases <- concat <$> mapM codeGenRuleContinuationEvaluate rules'
  let eval_int =
        if length phase_info == 1
          then ""
          else "\n\nevaluatePhased :: (RuleInstance, Phase) -> (Fact, Phase)\nevaluatePhased (r, p) = (evaluate r, nextPhase p)"
      eq_instance = "\n\ninstance Eq RuleInstance where\n  f == f'\n    | f < f' = False\n    | f' < f = False\n    | otherwise = True"
  pr <- fixenGetPriorities
  let priorities' = snd <$> IntMap.toList pr
  pr_code <- codeGenPriorities priorities'
  return $
    Text.concat
      [ "data RuleInstance\n       = "
      , (Text.intercalate "\n       | " rule_cases)
      , "\n       | Init Fact"
      , "\n  deriving Show\n\n"
      , "type Queue = Q.MaxQueue "
      , q_ty
      , eq_instance
      , pr_code
      , eval_decl
      , Text.intercalate "\n" eval_cases
      , "\nevaluate (Init f) = f"
      , eval_int
      ]

codeGenPriorities :: [PriorityInfo] -> FixenPass CodeGenState Text
codeGenPriorities ls = do
  let header = "\n\ninstance Ord RuleInstance where\n  i <= i' = not (i' < i)\n  ----- PRIORITIES -----\n  Init _ < Init _ = False\n  _ < Init _ = True\n"
  cases <- mapM codeGenPriority ls
  return $ Text.concat [header, Text.intercalate "\n" cases, "\n  _ < _ = False"]

codeGenPriority :: PriorityInfo -> FixenPass CodeGenState Text
codeGenPriority p_info = do
  let p = _priorityDeclaration p_info
      p_exp = priorityPremise p
      c = priorityConclusion p
      c_lhs = priorityConclusionLHS c
      c_rhs = priorityConclusionRHS c
      lhs_rule = ruleInstanceRule c_lhs
      lhs_vars = Map.fromList $ (\(t, t') -> (simpleIdentifier t, simpleIdentifier t')) <$> (Map.toList $ ruleInstanceMap c_lhs)
      rhs_rule = ruleInstanceRule c_rhs
      rhs_vars = Map.fromList $ (\(t, t') -> (simpleIdentifier t, simpleIdentifier t')) <$> (Map.toList $ ruleInstanceMap c_rhs)
      lhs_rule_name = Text.append "Rule" (capitalize $ simpleIdentifier lhs_rule)
      rhs_rule_name = Text.append "Rule" (capitalize $ simpleIdentifier rhs_rule)
      (lhs_rule_id, rhs_rule_id) = _priorityRules p_info
  rule_info_map <- fixenGetRuleInfo
  let lhs_rule_dec = rule_info_map IntMap.! lhs_rule_id
      lhs_bound_vars = Map.keys $ _ruleBoundVars lhs_rule_dec
      lhs_code_vars =
        ( \v -> case lhs_vars Map.!? v of
            Nothing -> "_"
            Just v' -> v'
        )
          <$> lhs_bound_vars
      rhs_rule_dec = rule_info_map IntMap.! rhs_rule_id
      rhs_bound_vars = Map.keys $ _ruleBoundVars rhs_rule_dec
      rhs_code_vars =
        ( \v -> case rhs_vars Map.!? v of
            Nothing -> "_"
            Just v' -> v'
        )
          <$> rhs_bound_vars

  return $
    Text.concat
      [ "  ("
      , lhs_rule_name
      , " "
      , Text.intercalate " " lhs_code_vars
      , ") < ("
      , rhs_rule_name
      , " "
      , Text.intercalate " " rhs_code_vars
      , ") = "
      , codeGenExprWithNameReplacement IntMap.empty Map.empty p_exp
      ]

codeGenRuleContinuationEvaluate :: (Int, RuleInfo) -> FixenPass CodeGenState [Text]
codeGenRuleContinuationEvaluate (i, r) = do
  n <- codeGenGetRuleContinuationName i
  let rul = _ruleDeclaration r
  if length (ruleAssumptions rul) == 0
    then return []
    else do
      let bv = Map.keys $ _ruleBoundVars r
          conc = ruleConclusion $ rul
          conc_name = conc ^. name
          conc_args = conc ^. args
          conc_args_code = codeGenExprWithNameReplacement IntMap.empty Map.empty <$> conc_args
      return $ [Text.concat ["evaluate (", n, " ", Text.intercalate " " bv, ") = ", simpleIdentifier conc_name, " ", Text.intercalate " " conc_args_code]]

codeGenGetRuleContinuationName :: Int -> FixenPass CodeGenState Text
codeGenGetRuleContinuationName i = do
  r_i <- fixenGetRuleInfo
  let rule_info = r_i IntMap.! i
      decl = _ruleDeclaration rule_info
  case ruleName decl of
    Nothing -> return $ Text.append "UnnamedRule" (Text.show i)
    Just v -> return $ Text.append "Rule" (capitalize $ simpleIdentifier v)

codeGenRuleContinuationType :: (Int, RuleInfo) -> FixenPass CodeGenState [Text]
codeGenRuleContinuationType (n, r) = do
  let rule_dec = _ruleDeclaration r
  if null (ruleAssumptions rule_dec)
    then return []
    else do
      name' <- codeGenGetRuleContinuationName n
      let bv = _ruleBoundVars r
          args' = _ruleParamType . snd <$> Map.toList bv
      arg_ty <- mapM fromTypeLattice args'
      arg_u_ty <- mapM getUnderlyingType arg_ty
      let arg_ty_code = codeGenType <$> arg_u_ty
      return [Text.intercalate " " (name' : arg_ty_code)]
  where
    fromTypeLattice Dynamic = failErr (Just "panic") "something went wrong; dynamic type found in codegen!" [] []
    fromTypeLattice (ActualType t _) = return t
    fromTypeLattice Bottom = failErr (Just "panic") "something went wrong; bottom type found in codegen!" [] []
capitalize :: Text -> Text
capitalize t = case Text.uncons t of
  Just (c, t') -> Text.cons (Char.toUpper c) t'
  Nothing -> t

codeGenHsBlocks :: [HsBlock] -> Text
codeGenHsBlocks [] = ""
codeGenHsBlocks ls =
  let t = Text.intercalate "\n\n" $ (Text.strip . hsBlockContents) <$> ls
   in Text.concat ["\n----- USER CODE START -----\n", t, "\n----- USER CODE END -----"]

codeGenModuleDeclaration :: ModuleDeclaration -> Text
codeGenModuleDeclaration m =
  let n = moduleDeclarationName m
   in Text.concat ["module ", fullIdentifier n, " where"]

codeGenImportStmt :: HsImport -> Text
codeGenImportStmt i =
  let n = i ^. moduleName
   in Text.concat ["import ", fullIdentifier n]

codeGenEntailment :: RelationRepresentation -> FixenPass CodeGenState Text
codeGenEntailment rep = do
  let ty_decl = "infix 0 |=\n\n(|=) :: Database -> Fact -> Bool"
      all_cases = Map.toList rep
  all_cases_code <- mapM codeGenEntailmentCase all_cases
  return $ Text.intercalate "\n" (ty_decl : all_cases_code)

codeGenEntailmentCase :: (Text, RelationRepresentationInfo) -> FixenPass CodeGenState Text
codeGenEntailmentCase (name', rep_info) = do
  if null db_ty
    then return header
    else
      if length steps_individual_code <= 1
        then return $ Text.concat (header : steps_individual_code)
        else return $ Text.concat [header, "fromMaybe False $ do\n        ", (Text.intercalate "\n        " (init steps_individual_code)), "\n        return $ ", last steps_individual_code]
  where
    db_rep = _databaseRepresentation rep_info
    e_map = _extractionMap db_rep
    db_ty = _databaseTypes db_rep
    fact_rep = _factRepresentation rep_info
    fact_ty = _factTypes fact_rep
    case_vars = (Text.append "_v") <$> Text.show <$> [0 .. length fact_ty - 1]
    header = if null db_ty then Text.concat ["db |= ", name', " = _facts", name', " db"] else Text.concat ["db |= ", "(", (Text.intercalate " " (name' : case_vars)), ") =\n  let db' = _facts", name', " db\n   in "]
    extraction_order = (e_map IntMap.!) <$> [0 .. length fact_ty - 1]
    extraction_proc = zipWith (\x (y, _, z) -> (x, y, z)) extraction_order db_ty
    steps_individual_code = steps 0 extraction_proc
    steps :: Int -> [(Int, QueryType, Type)] -> [Text]
    steps _ [] = [] -- no one cares; this case never happens anyway!
    steps step_no ls@((idx, Meet l _, _) : _) =
      let n = length ls -- how big is this tuple? is it standalone?
          curr_db = getCurrDb step_no
       in if n == 1
            then -- HashSet (ty). Just look for anything inside that subsumes the var
              [Text.concat ["any (", codeGenIdentifier l, " _v", Text.show idx, ") ", curr_db]]
            else -- HashSet (ty1, ty2, ...). Use `any` to look through stuff.

              let fn_params = Text.append "_t" <$> Text.show <$> [0 .. n - 1]
                  remaining_vars_idx = fmap (\(i, _, _) -> i) ls
                  remaining_vars_leq =
                    fmap
                      ( \(_, l', _) -> case l' of
                          Match -> "(==)"
                          Meet x _ -> codeGenIdentifier x
                      )
                      ls
                  remaining_vars = Text.append "_v" <$> Text.show <$> remaining_vars_idx
                  fn_arg_tup = Text.intercalate ", " fn_params
                  fn_header = Text.concat ["\\(", fn_arg_tup, ") -> "]
                  fn_body_components = zipWith3 (\t v leq' -> Text.concat ["(", leq', " ", v, " ", t, ")"]) fn_params remaining_vars remaining_vars_leq
                  fn_body = Text.intercalate " && " fn_body_components
                  any_fn = Text.concat ["(", fn_header, fn_body, ")"]
               in [Text.concat ["any ", any_fn, " ", curr_db]]
    steps step_no [(idx, Match, _)] =
      -- HashSet of some discrete stuff. Just check element membership.
      [Text.concat ["_v", Text.show idx, " `HashSet.member` step", Text.show step_no]]
    steps step_no ((idx, Match, _) : xs) =
      -- lookup from the hashmap
      let curr_db = getCurrDb step_no
          curr_step = Text.concat ["step", Text.show (step_no + 1), " <- ", curr_db, " HashMap.!? _v", Text.show idx]
       in curr_step : steps (step_no + 1) xs
    getCurrDb 0 = "db'"
    getCurrDb n = Text.append "step" (Text.show n)

codeGenFactInsertions :: RelationRepresentation -> FixenPass CodeGenState Text
codeGenFactInsertions rep = do
  let ty_decl = "insertToDb :: Database -> Fact -> Maybe Database\ninsertToDb db fact\n  | db |= fact = Nothing"
      all_cases = Map.toList rep
  all_cases_code <- mapM codeGenFactInsertionCase all_cases
  phases' <- fixenGetPhases
  if NonEmpty.length phases' == 1
    then return $ Text.intercalate "\n" (ty_decl : all_cases_code)
    else
      return $
        Text.concat
          [ Text.intercalate "\n" (ty_decl : all_cases_code)
          , "\n\ninsertToInterpretation :: Interpretation -> Fact -> Phase -> Maybe Interpretation"
          , "\ninsertToInterpretation i f p = do\n"
          , "  let db = selectDb i p\n"
          , "  db' <- insertToDb db f\n"
          , "  return (replaceDb i db' p)"
          ]

codeGenFactInsertionCase :: (Text, RelationRepresentationInfo) -> FixenPass CodeGenState Text
codeGenFactInsertionCase (name', rep_info) = do
  if null db_ty
    then return header
    else return $ Text.concat [header, steps_individual_code, new_mp_code]
  where
    db_rep = _databaseRepresentation rep_info
    db_ty = _databaseTypes db_rep
    fact_rep = _factRepresentation rep_info
    fact_ty = _factTypes fact_rep
    i_map = _extractionMap db_rep
    case_vars = (Text.append "_v") <$> Text.show <$> [0 .. length fact_ty - 1]
    header =
      if null db_ty
        then
          Text.concat
            [ "insertToDb db "
            , name'
            , " = Just db { _facts"
            , name'
            , " = True }"
            ]
        else
          Text.concat
            [ "insertToDb db "
            , "("
            , (Text.intercalate " " (name' : case_vars))
            , ") =\n  let mp = _facts"
            , name'
            , " db\n      new_fact = "
            ]
    insertion_order = (i_map IntMap.!) <$> [0 .. length fact_ty - 1]
    extraction_proc = zipWith (\x (y, _, z) -> (x, y, z)) insertion_order db_ty
    steps_individual_code = steps extraction_proc
    new_mp_code =
      Text.concat
        [ "\n      mp' = "
        , insertionFn 0 extraction_proc
        , "\n              new_fact\n              mp\n   in Just db { _facts"
        , name'
        , " = mp' }"
        ]
    steps :: [(Int, QueryType, Type)] -> Text
    steps [] = "" -- no one cares; this case never happens anyway!
    steps ls@((idx, Meet _ _, _) : _) =
      if length ls == 1
        then -- just a hashset
          Text.concat ["HashSet.singleton _v", Text.show idx]
        else
          let remaining_vars_idx = fmap (\(i, _, _) -> i) ls
              remaining_vars_vars = fmap (\i -> Text.append "_v" (Text.show i)) remaining_vars_idx
              tup_components = Text.intercalate ", " remaining_vars_vars
           in Text.concat ["HashSet.singleton (", tup_components, ")"]
    steps [(idx, _, _)] = Text.concat ["HashSet.singleton _v", Text.show idx]
    steps ((idx, _, _) : xs) = Text.concat ["HashMap.singleton _v", Text.show idx, " (", steps xs, ")"]
    insertionFn :: Int -> [(Int, QueryType, Type)] -> Text
    insertionFn _ [] = "" -- no one cares; this case never happens anyway!
    insertionFn indent ls@((idx, Meet l _, _) : _) =
      let n = length ls
       in if n == 1
            then
              Text.concat
                [ "(\\s1 s2 ->"
                , indentation (indent + 2)
                , "HashSet.union"
                , indentation (indent + 3)
                , "s1"
                , indentation (indent + 3)
                , "(HashSet.filter"
                , indentation (indent + 4)
                , "(\\_t -> not (_t /= _v"
                , Text.show idx
                , " && "
                , codeGenIdentifier l
                , " _t _v"
                , Text.show idx
                , "))"
                , indentation (indent + 4)
                , "s2))"
                ]
            else
              let db_vars = Text.append "_t" <$> Text.show <$> [0 .. n - 1]
                  filter_fn_hd =
                    Text.concat
                      [ indentation (indent + 4)
                      , "(\\("
                      , Text.intercalate ", " db_vars
                      , ") -> not ("
                      ]
                  remaining_vars_idx = fmap (\(i, _, _) -> i) ls
                  remaining_vars_leq =
                    fmap
                      ( \(_, l', _) -> case l' of
                          Match -> "(==)"
                          Meet x _ -> codeGenIdentifier x
                      )
                      ls
                  remaining_vars = Text.append "_v" <$> Text.show <$> remaining_vars_idx
                  fn_body_components = zipWith3 (\t v leq' -> Text.concat [t, " /= ", v, " && ", leq', " ", t, " ", v]) db_vars remaining_vars remaining_vars_leq
                  fn_body = Text.intercalate " && " fn_body_components
                  any_fn =
                    Text.concat
                      [ "(\\s1 s2 ->"
                      , indentation (indent + 2)
                      , "HashSet.union"
                      , indentation (indent + 3)
                      , "s1"
                      , indentation (indent + 3)
                      , "(HashSet.filter"
                      , filter_fn_hd
                      , fn_body
                      , "))"
                      , indentation (indent + 4)
                      , "s2))"
                      ]
               in any_fn
    insertionFn _ [(_, Match, _)] = "HashSet.union"
    insertionFn indent (_ : xs) = Text.concat ["HashMap.unionWith", indentation (indent + 1), "(", insertionFn (indent + 1) xs, ")"]
    indentation 0 = " "
    indentation n = Text.cons '\n' $ Text.replicate (12 + (n * 2)) " "

codeGenEmptyDb :: RelationRepresentation -> Text
codeGenEmptyDb r =
  let hd = "emptyDb :: Database\nemptyDb = Database\n  { "
      facts = Map.toList r
      fds = codeGenEmptyDbArgs <$> facts
      all_fds = Text.intercalate "\n  , " fds
   in Text.concat [hd, all_fds, "\n  }"]

codeGenEmptyDbArgs :: (Text, RelationRepresentationInfo) -> Text
codeGenEmptyDbArgs (t, r) =
  let field_name = Text.append "_facts" t
      db_rep = _databaseRepresentation r
      db_ty = _databaseTypes db_rep
   in case db_ty of
        [] -> Text.concat [field_name, " = False"]
        [_] -> Text.concat [field_name, " = HashSet.empty"]
        ((Meet _ _, _, _) : _) -> Text.concat [field_name, " = HashSet.empty"]
        _ -> Text.concat [field_name, " = HashMap.empty"]

codeGenDb :: RelationRepresentation -> Text
codeGenDb r =
  let facts = Map.toList r
      header = "data Database = Database\n  { "
      db_fields = Text.intercalate "\n  , " (codeGenDbField <$> facts)
   in Text.concat [header, db_fields, "\n  } deriving Eq"]

codeGenDbField :: (Text, RelationRepresentationInfo) -> Text
codeGenDbField (t, r) =
  let db_rep = _databaseRepresentation r
      db_ty = _databaseTypes db_rep
      db_ty_code = codeGenDbTypeArg db_ty
   in Text.concat ["_facts", t, " :: ", db_ty_code]

codeGenDbTypes :: RelationRepresentation -> Text
codeGenDbTypes r =
  let facts = Map.toList r
   in Text.intercalate "\n" $ codeGenDbType <$> facts

codeGenDbType :: (Text, RelationRepresentationInfo) -> Text
codeGenDbType (t, r) =
  let type_name = Text.append t "Facts"
      db_rep = _databaseRepresentation r
      db_ty = _databaseTypes db_rep
      db_ty_code = codeGenDbTypeArg db_ty
   in Text.concat ["type ", type_name, " = ", db_ty_code]

codeGenDbTypeArg :: [(QueryType, StoreType, Type)] -> Text
codeGenDbTypeArg [] = "Bool"
codeGenDbTypeArg [(_, _, x)] = Text.append "HashSet " (codeGenType x)
codeGenDbTypeArg ((Meet _ _, _, t) : x : xs) =
  let remaining_types = (\(_, _, x') -> x') <$> (x :| xs)
   in Text.append "HashSet " (codeGenType (TypeTuple 0 t remaining_types))
codeGenDbTypeArg ((Match, _, t) : xs) = Text.concat ["HashMap ", codeGenType t, " (", codeGenDbTypeArg xs, ")"]

codeGenFacts :: RelationRepresentation -> Text
codeGenFacts r =
  let header :: Text = "data Fact = "
      facts = Map.toList r
   in Text.concat [header, Text.intercalate "\n          | " (codeGenFact <$> facts), "\n  deriving (Show, Eq)"]

codeGenFact :: (Text, RelationRepresentationInfo) -> Text
codeGenFact (t, r) =
  let fact_rep = _factRepresentation r
      fact_ty = snd <$> _factTypes fact_rep
      ty_code = codeGenType <$> fact_ty
   in Text.intercalate " " (t : ty_code)

codeGenType :: Type -> Text
codeGenType (TypeName _ n) = codeGenIdentifier n
codeGenType (TypeApp _ lhs' rhs') = Text.concat ["(", codeGenType lhs', " ", codeGenType rhs', ")"]
codeGenType (TypeList _ t) = Text.concat ["[", codeGenType t, "]"]
codeGenType (TypeTuple _ hd tl) = Text.concat ["(", codeGenType hd, ", ", (Text.intercalate ", " (codeGenType <$> (NonEmpty.toList tl))), ")"]
codeGenType (TypeNatLit _ i) = Text.show i
codeGenType (TypeSymbolLit _ s) = Text.show s
codeGenType (TypeUnit _) = "()"

codeGenIdentifier :: Identifier -> Text
codeGenIdentifier i =
  if any (`elem` opChars) (Text.unpack $ simpleIdentifier i)
    then Text.concat ["(", fullIdentifier i, ")"]
    else fullIdentifier i

codeGenInterpretation :: FixenPass CodeGenState [Text]
codeGenInterpretation = do
  phases' <- fixenGetPhases
  let n = NonEmpty.length phases'
  if n == 1
    then return []
    else do
      let t = Text.intercalate ", " (replicate n "Database")
          p =
            Text.concat
              [ "\n\ndata Phase = "
              , (Text.intercalate "\n           | " (Text.append "Phase" . Text.show <$> [1 .. n]))
              , "\n deriving (Eq, Show, Ord)"
              ]
          nextPhases = Text.intercalate "\n" $ (\i -> Text.concat ["nextPhase Phase", Text.show i, " = Phase", Text.show (mod i n + 1)]) <$> [1 .. n]
          selectorPhases = Text.intercalate "\n" $ (`Text.append` " = db") . selectLhsTup n <$> [1 .. n]
          selector = Text.concat ["\n\nselectDb :: Interpretation -> Phase -> Database\n", selectorPhases]
          insertionPhases = Text.intercalate "\n" $ insertCase n <$> [1 .. n]
          insert_code = Text.concat ["\n\nreplaceDb :: Interpretation -> Database -> Phase -> Interpretation\n", insertionPhases]
          ent = Text.concat ["\n\n(||=) :: Interpretation -> Fact -> Phase -> Bool\n(i ||= f) p = selectDb i p |= f\n\ninfix 1 ||="]
      return
        [ Text.concat
            [ "\ntype Interpretation = ("
            , t
            , ")"
            , "\n\nemptyInterpretation :: Interpretation\nemptyInterpretation = ("
            , Text.intercalate ", " (replicate n "emptyDb")
            , ")"
            , p
            , "\n\nnextPhase :: Phase -> Phase\n"
            , nextPhases
            , selector
            , ent
            , insert_code
            ]
        ]
  where
    selectLhsTup n' i = Text.concat ["selectDb (", Text.intercalate ", " $ (replicate (i - 1) "_") ++ ("db" : replicate (n' - i) "_"), ") Phase", Text.show i]
    insertLhsTup n' i =
      let components = (\i' -> if i == i' then "_" else Text.concat ["db", Text.show i']) <$> [1 .. n']
       in Text.concat ["replaceDb (", Text.intercalate ", " components, ")"]
    insertRhsTup n' i =
      let components = (\i' -> if i == i' then "db'" else Text.concat ["db", Text.show i']) <$> [1 .. n']
       in Text.concat ["(", Text.intercalate ", " components, ")"]
    insertCase n' i = Text.concat [insertLhsTup n' i, " db' Phase", Text.show i, " = ", insertRhsTup n' i]
