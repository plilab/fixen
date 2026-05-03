{-# LANGUAGE OverloadedStrings #-}

module Fixen.CodeGen where

import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Fixen.Data.NodeId
import Fixen.IR.AST
import Fixen.IR.RelationRepresentation
import Fixen.IR.RuleForest (RuleForest)
import Fixen.Monad
import Fixen.Parser.Token (opChars)

type CodeGenState = SymbolEnv :*: PositionEnv :*: NodeId :*: FixenErrors

codeGen :: NonEmpty RuleForest -> RelationRepresentation -> Program -> FixenPass CodeGenState Text
codeGen _ relation_rep prog = do
  let mod_head = moduleName prog
      std_impts = "\n----- FIXEN IMPORTS-----\nimport Data.Maybe\nimport Data.HashSet (HashSet)\nimport qualified Data.HashSet as HashSet\nimport Data.HashMap.Strict (HashMap)\nimport qualified Data.HashMap.Strict as HashMap"
      mod_head_code = codeGenModuleDeclaration mod_head
      import_code = if null (hsImports prog) then "" else Text.append "\n----- USER IMPORTS -----\n" $ Text.intercalate "\n" $ codeGenImportStmt <$> hsImports prog
      hs_blocks_code = codeGenHsBlocks $ hsBlocks prog
      fact_code = codeGenFacts relation_rep
      db_types_code = codeGenDbTypes relation_rep
      db_code = codeGenDb relation_rep
  interpretation_code <- codeGenInterpretation
  entailment_code <- codeGenEntailment relation_rep
  return $
    Text.intercalate
      "\n"
      [ mod_head_code
      , std_impts
      , import_code
      , hs_blocks_code
      , "\n----- FACTS -----"
      , fact_code
      , "\n----- FACT DATABASE -----"
      , db_types_code
      , ""
      , db_code
      , interpretation_code
      , "\n----- ENTAILMENT -----"
      , entailment_code
      ]

-- codeGenSubsumption :: FixenPass CodeGenState Text
-- codeGenSubsumption = do
--   -- the class declaration itself
--   rels <- fixenGetRelationInfo
--   -- create the instances for relation arguments
--   let all_types =
--         Map.keys $ Map.unions $ (\i -> Map.singleton (calculateRepresentativeFromType i) i) <$> (concat $ relationParams <$> _relationDeclaration <$> snd <$> Map.toList rels)
--   arg_inst <- concat <$> mapM codeGenSubsumptionRelationArg all_types
--   let subsume_cls_decl = "class Subsumable a where\n  subsumedBy :: a -> a -> Bool\n\nsubsumes :: Subsumable a => a -> a -> Bool\nsubsumes = flip subsumedBy"
--   let cls_decl = if null arg_inst then subsume_cls_decl else Text.append subsume_cls_decl "\n\nclass MLB a where\n  mlbs :: a -> a -> [a]"
--   arg_inst_code <- Text.intercalate "\n\n" <$> concat <$> mapM codeGenSubsumptionRelationArg all_types
--   -- create the instances for facts
--   let all_rels = Map.toList rels
--   fact_inst <- codeGenSubsumptionFact all_rels
--   return $
--     if null arg_inst
--       then (Text.intercalate "\n\n" [cls_decl, fact_inst])
--       else
--         Text.intercalate
--           "\n\n"
--           [ cls_decl
--           , arg_inst_code
--           , fact_inst
--           ]

-- codeGenSubsumptionFact :: [(Name, RelationInfo)] -> FixenPass CodeGenState Text
-- codeGenSubsumptionFact ls = do
--   cases <- mapM codeGenSubsumptionFactCase ls
--   let all_cases = cases ++ ["subsumedBy _ _ = False"]
--   return $ Text.intercalate "\n  " ("instance Subsumable Fact where" : all_cases)
--
-- codeGenSubsumptionFactCase :: (Name, RelationInfo) -> FixenPass CodeGenState Text
-- codeGenSubsumptionFactCase (name, rel_info) = do
--   let r = _relationDeclaration rel_info
--       p = relationParams r
--   k <- mapM fixenGetRelationParamKind p
--   let lhs_names = map (\i -> Text.append "_v" (Text.show i)) [0 .. length p - 1]
--       lhs_case = Text.concat ["(", Text.intercalate " " (name : lhs_names), ")"]
--       rhs_names = map (\i -> Text.concat ["_v", (Text.show i), "'"]) [0 .. length p - 1]
--       rhs_case = Text.concat ["(", Text.intercalate " " (name : rhs_names), ")"]
--       e_info = zip3 lhs_names rhs_names k
--       e_terms =
--         ( \(x, y, k') -> case k' of
--             Discrete -> Text.intercalate " " [x, "==", y]
--             PartiallyOrdered -> Text.concat ["(", Text.intercalate " " [x, "`subsumedBy`", y], ")"]
--         )
--           <$> e_info
--       expr = if null p then "True" else Text.intercalate "\n      && " e_terms
--   return $ Text.intercalate " " ["subsumedBy", lhs_case, rhs_case, "=\n   ", expr]
--
-- codeGenSubsumptionRelationArg :: Text -> FixenPass CodeGenState [Text]
-- codeGenSubsumptionRelationArg name = do
--   k <- fixenGetRelationParamKindFromName name
--   case k of
--     Discrete -> return []
--     PartiallyOrdered -> do
--       partial_ord_info <- fixenGetPartialOrdInfo
--       let p_ord = partial_ord_info Map.! name
--           u_ty = partialOrdDeclarationType p_ord
--           u_leq = partialOrdDeclarationLeq p_ord
--           u_mlb = partialOrdDeclarationMlbs p_ord
--       return
--         [ Text.concat
--             [ "instance Subsumable "
--             , codeGenType u_ty
--             , " where\n  subsumedBy = "
--             , codeGenIdentifier u_leq
--             , "\n\ninstance MLB "
--             , codeGenType u_ty
--             , " where\n  mlbs = "
--             , codeGenIdentifier u_mlb
--             ]
--         ]

codeGenHsBlocks :: [HsBlock] -> Text
codeGenHsBlocks [] = ""
codeGenHsBlocks ls =
  let t = Text.intercalate "\n\n" $ (Text.strip . hsBlockContents) <$> ls
   in Text.append "----- USER CODE -----\n" t

codeGenModuleDeclaration :: ModuleDeclaration -> Text
codeGenModuleDeclaration m =
  let n = moduleDeclarationName m
   in Text.concat ["module ", fullIdentifier n, " where"]

codeGenImportStmt :: HsImport -> Text
codeGenImportStmt i =
  let n = hsImportImport i
   in Text.concat ["import ", fullIdentifier n]

codeGenDb :: RelationRepresentation -> Text
codeGenDb r =
  let facts = fst <$> Map.toList r
      header = "data Database = Database\n  { "
      db_fields = Text.intercalate "\n  , " (f <$> facts)
   in Text.concat [header, db_fields, "\n  } deriving Eq"]
  where
    f x = Text.concat ["_facts", x, " :: ", x, "Facts"]

codeGenEntailment :: RelationRepresentation -> FixenPass CodeGenState Text
codeGenEntailment rep = do
  let ty_decl = "infix 0 |=\n\n(|=) :: Database -> Fact -> Bool"
      all_cases = Map.toList rep
  all_cases_code <- mapM codeGenEntailmentCase all_cases
  return $ Text.intercalate "\n" (ty_decl : all_cases_code)

codeGenEntailmentCase :: (Text, RelationRepresentationInfo) -> FixenPass CodeGenState Text
codeGenEntailmentCase (name, rep_info) = do
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
    header = if null db_ty then Text.concat ["db |= ", name, " = _facts", name, " db"] else Text.concat ["db |= ", "(", (Text.intercalate " " (name : case_vars)), ") =\n  let db' = _facts", name, " db\n   in "]
    extraction_order = (e_map IntMap.!) <$> [0 .. length fact_ty - 1]
    extraction_proc = zipWith (\x (y, z) -> (x, y, z)) extraction_order db_ty
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
                  remaining_vars_idx = map (\(i, _, _) -> i) ls
                  remaining_vars_leq =
                    map
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

codeGenDbTypeArg :: [(QueryType, Type)] -> Text
codeGenDbTypeArg [] = "Bool"
codeGenDbTypeArg [(_, x)] = Text.append "HashSet " (codeGenType x)
codeGenDbTypeArg ((Meet _ _, t) : x : xs) =
  let remaining_types = snd <$> (x :| xs)
   in Text.append "HashSet " (codeGenType (TypeTuple 0 t remaining_types))
codeGenDbTypeArg ((Match, t) : xs) = Text.concat ["HashMap ", codeGenType t, " (", codeGenDbTypeArg xs, ")"]

codeGenFacts :: RelationRepresentation -> Text
codeGenFacts r =
  let header :: Text = "data Fact = "
      facts = Map.toList r
   in Text.concat [header, Text.intercalate "\n          | " (codeGenFact <$> facts), "\n  deriving Eq"]

codeGenFact :: (Text, RelationRepresentationInfo) -> Text
codeGenFact (t, r) =
  let fact_rep = _factRepresentation r
      fact_ty = _factTypes fact_rep
      ty_code = codeGenType <$> fact_ty
   in Text.intercalate " " (t : ty_code)

codeGenType :: Type -> Text
codeGenType (TypeName _ n) = codeGenIdentifier n
codeGenType (TypeApp _ lhs rhs) = Text.concat ["(", codeGenType lhs, " ", codeGenType rhs, ")"]
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

codeGenInterpretation :: FixenPass CodeGenState Text
codeGenInterpretation = do
  phases <- fixenGetPhases
  let n = NonEmpty.length phases
  if n == 1
    then return "\ntype Interpretation = Database"
    else do
      let t = Text.intercalate ", " (replicate n "Database")
      return $ Text.concat ["\ntype Interpretation = (", t, ")"]
