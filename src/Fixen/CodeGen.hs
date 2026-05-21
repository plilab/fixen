{-# LANGUAGE OverloadedStrings #-}

module Fixen.CodeGen where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Fixen.CodeGen.Common
import Fixen.CodeGen.Database
import Fixen.CodeGen.Fact
import Fixen.CodeGen.HsBlock
import Fixen.CodeGen.Import
import Fixen.CodeGen.ModuleDeclaration
import Fixen.CodeGen.MultiPhase
import Fixen.CodeGen.RuleInstance
import Fixen.Fields
import Fixen.IR.AST
import Fixen.IR.RelationRepresentation
import Fixen.IR.RuleForest
import Fixen.Monad

codeGen :: NonEmpty RuleForest -> RelationRepresentation -> Program -> FixenPass CodeGenState Text
codeGen forest relation_rep prog = do
  let mod_head_code = codeGenModuleDeclaration prog
      import_code = codeGenImports prog
      hs_blocks_code = codeGenHsBlocks prog
      fact_code = codeGenFacts relation_rep
  db_code <- codeGenDb relation_rep
  cont_code <- codeGenRuleInstance
  step_code <- codeGenStep forest relation_rep
  loop_and_solve_code <- codeGenLoopAndSolve
  re_solve_code <- codeGenReSolve forest
  q_code <- mapM (codeGenQuery relation_rep) (prog ^. queries)
  multi_phase <- codeGenMultiPhase
  return
    $ Text.intercalate
      "\n\n"
    $ [ mod_head_code
      , import_code
      , hs_blocks_code
      , fact_code
      , db_code
      , cont_code
      , "\n----- STEP FUNCTION -----"
      , step_code
      , "\n----- SOLVER -----"
      , loop_and_solve_code
      , re_solve_code
      , "\n----- QUERIES -----"
      , Text.intercalate "\n\n" q_code
      , multi_phase
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
          , Text.intercalate " -> " (ty_sig ++ ["Database", "[Fact]"])
          , "\n"
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
            remaining <- codeGenQueryStep (length ty') rel_rep (IntMap.unionsWith max [name_supply, IntMap.fromList [(i, 0) | i <- [n .. length ty' - 1]], IntMap.fromList [(i, 1) | i <- need_to_leq]]) rel_name
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
      let leaves' = _ruleForestLeaves x
      l_c <- mapM (codeGenFactLeaves Nothing) leaves'
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
      leaves' = _ruleForestLeaves forest
  if length branches + length leaves' <= 1
    then -- continue in the straight line

      if length branches == 0
        then do
          leaves_code <- mapM (codeGenSinglePhaseLeaves name_supply indent phase_no) leaves'
          return $ Text.concat leaves_code
        else do
          branches_code <- mapM (codeGenSinglePhaseBranch rel_rep name_supply indent 0 phase_no) branches
          return $ Text.concat branches_code
    else do
      leaves_code <- mapM (codeGenSinglePhaseLeaves name_supply (indent + 1) phase_no) leaves'
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

codeGenGetRuleContinuationName :: Int -> FixenPass CodeGenState Text
codeGenGetRuleContinuationName i = do
  r_i <- fixenGetRuleInfo
  let rule_info = r_i IntMap.! i
      decl = _ruleDeclaration rule_info
  case ruleName decl of
    Nothing -> return $ Text.append "UnnamedRule" (Text.show i)
    Just v -> return $ Text.append "Rule" (capitalize $ simpleIdentifier v)
