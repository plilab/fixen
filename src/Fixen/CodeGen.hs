{-# LANGUAGE OverloadedStrings #-}

module Fixen.CodeGen where

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
      mod_head_code = codeGenModuleDeclaration mod_head
      import_code = Text.intercalate "\n" $ codeGenImportStmt <$> hsImports prog
      hs_blocks_code = codeGenHsBlocks $ hsBlocks prog
      fact_code = codeGenFacts relation_rep
      db_types_code = codeGenDbTypes relation_rep
      db_code = codeGenDb relation_rep
  interpretation_code <- codeGenInterpretation
  subsumption_inst_code <- codeGenSubsumption
  return $
    Text.intercalate
      "\n"
      [ mod_head_code
      , import_code
      , hs_blocks_code
      , "\n----- FACTS -----"
      , fact_code
      , "\n----- Database Representations -----"
      , db_types_code
      , "\n----- Database -----"
      , db_code
      , interpretation_code
      , "\n----- Subsumption -----"
      , subsumption_inst_code
      ]

codeGenSubsumption :: FixenPass CodeGenState Text
codeGenSubsumption = do
  let class_decl = "class Subsumable a where\n    subsumes :: a -> a -> Bool\n    mlbs :: a -> a -> [a]"
  rels <- fixenGetRelationInfo
  let all_types =
        Map.toList $ Map.unions $ (\i -> Map.singleton (calculateRepresentativeFromType i) i) <$> (concat $ relationParams <$> _relationDeclaration <$> snd <$> Map.toList rels)
  arg_inst <- Text.intercalate "\n\n" <$> mapM codeGenSubsumptionRelationArg all_types
  return $
    Text.intercalate
      "\n\n"
      [ class_decl
      , arg_inst
      ]

codeGenSubsumptionRelationArg :: (Text, Type) -> FixenPass CodeGenState Text
codeGenSubsumptionRelationArg (name, ty) = do
  info <- fixenGetRelationParamKindInfo
  let k = info Map.! name
  case k of
    Discrete -> return $ Text.concat ["instance Subsumable ", codeGenType ty, " where\n  subsumes = (==)\n  mlbs _a _b\n    | _a == _b = [_a]\n    | otherwise = []"]
    PartiallyOrdered -> do
      partial_ord_info <- fixenGetPartialOrdInfo
      let p_ord = partial_ord_info Map.! name
          u_ty = partialOrdDeclarationType p_ord
          u_leq = partialOrdDeclarationLeq p_ord
          u_mlb = partialOrdDeclarationMlbs p_ord
      return $
        Text.concat
          [ "instance Subsumable "
          , codeGenType u_ty
          , " where\n  subsumes = "
          , codeGenIdentifier u_leq
          , "\n  mlbs = "
          , codeGenIdentifier u_mlb
          ]

codeGenHsBlocks :: [HsBlock] -> Text
codeGenHsBlocks [] = ""
codeGenHsBlocks ls =
  let t = Text.intercalate "\n\n" $ (Text.strip . hsBlockContents) <$> ls
   in Text.concat ["\n----- USER CODE START -----\n", t, "\n----- USER CODE END -----\n"]

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
codeGenDbTypeArg ((Meet _, t) : x : xs) =
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
