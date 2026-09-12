{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Haskell storage declarations, entailment, insertion and contour merging.
-- Each traversal carries its current index explicitly and produces syntax
-- trees. No traversal depends on indentation or a sentinel variable ID.
module Fixen.CodeGen.Database where

import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Fixen.CodeGen.Common
import Fixen.CodeGen.Fact (comparisonName)
import Fixen.CodeGen.Haskell.Bindings (numberedName)
import Fixen.CodeGen.Haskell.Syntax qualified as Hs
import Fixen.IR.AST (Type)
import Fixen.IR.RelationRepresentation
import Fixen.Monad

codeGenDb :: RelationRepresentation -> FixenPass CodeGenState Text
codeGenDb layouts =
  pure $
    Text.intercalate
      "\n\n"
      [ codeGenDbDef layouts
      , codeGenEmptyDb layouts
      , "infix 0 |=\n"
          <> Hs.renderDecls
            (signature "|=" ["Database", "Fact"] (Hs.typ "Bool") : (entailmentCase <$> relations))
      , Hs.renderDecls (signature "insertToDb" ["Database", "Fact"] (Hs.typ "Database") : (insertionCase <$> relations))
      , codeGenMergeContour layouts
      ]
  where
    relations = Map.toList layouts

signature :: Text -> [Text] -> Hs.Type -> Hs.Decl
signature n args = Hs.Signature (Hs.name n) . Hs.functionType (Hs.typ <$> args)

codeGenDbDef :: RelationRepresentation -> Text
codeGenDbDef layouts =
  Hs.renderDecls
    [ Hs.Data
        (Hs.name "Database")
        ( Hs.RecordConstructor
            (Hs.name "Database")
            [(Hs.name (dbFactSelector rel), buildDbFieldType True (_databaseTypes (_databaseRepresentation layout))) | (rel, layout) <- Map.toList layouts]
            :| []
        )
        [Hs.name "Eq"]
    ]

-- | The root lattice suffix is optional; under a discrete key it is present
-- whenever that key is present. Partial-order suffixes are antichain sets.
buildDbFieldType :: Bool -> [(QueryType, StoreType, Type)] -> Hs.Type
buildDbFieldType _ [] = Hs.typ "Bool"
buildDbFieldType root fields@((LatticeMeet {}, _, _) : _) =
  let suffix = Hs.tupleType [lowerType t | (_, _, t) <- fields]
   in if root then Hs.TyApp (Hs.typ "Maybe") suffix else suffix
buildDbFieldType _ fields@((Meet {}, _, _) : _) =
  Hs.TyApp (Hs.typ "HashSet") (Hs.tupleType [lowerType t | (_, _, t) <- fields])
buildDbFieldType _ [(_, _, t)] = Hs.TyApp (Hs.typ "HashSet") (lowerType t)
buildDbFieldType _ ((_, _, t) : rest) =
  Hs.TyApp (Hs.TyApp (Hs.typ "HashMap") (lowerType t)) (buildDbFieldType False rest)

codeGenEmptyDb :: RelationRepresentation -> Text
codeGenEmptyDb layouts =
  Hs.renderDecls
    [ Hs.Signature (Hs.name "emptyDb") (Hs.typ "Database")
    , Hs.Function
        (Hs.name "emptyDb")
        []
        ( Hs.Record
            (Hs.name "Database")
            [(Hs.name (dbFactSelector rel), emptyIndex (_databaseTypes (_databaseRepresentation layout))) | (rel, layout) <- Map.toList layouts]
        )
    ]
  where
    emptyIndex [] = Hs.var "False"
    emptyIndex ((LatticeMeet {}, _, _) : _) = Hs.var "Nothing"
    emptyIndex ((_, StoredAsHashMap, _) : _) = Hs.var "HashMap.empty"
    emptyIndex _ = Hs.var "HashSet.empty"

-- | Fields in storage order, carrying their original fact argument positions.
storageFields :: RelationRepresentationInfo -> [(Int, QueryType)]
storageFields layout =
  zip
    (IntMap.elems (_extractionMap (_databaseRepresentation layout)))
    [q | (q, _, _) <- _databaseTypes (_databaseRepresentation layout)]

factPattern :: Text -> Int -> Hs.Pattern
factPattern rel arity = Hs.PCon (Hs.name rel) [Hs.PVar (valueName i) | i <- [0 .. arity - 1]]

valueName :: Int -> Hs.Name
valueName = numberedName "_v"

storedName :: Int -> Hs.Name
storedName = numberedName "_t"

value :: Int -> Hs.Expr
value = Hs.Var . valueName

stored :: Int -> Hs.Expr
stored = Hs.Var . storedName

indexExpr :: Text -> Hs.Expr
indexExpr rel = Hs.call (dbFactSelector rel) [Hs.var "db"]

entailmentCase :: (Text, RelationRepresentationInfo) -> Hs.Decl
entailmentCase (rel, layout) =
  Hs.Function
    (Hs.name "|=")
    [Hs.pat "db", factPattern rel (length fields)]
    (entails True 0 (indexExpr rel) fields)
  where
    fields = storageFields layout

entails :: Bool -> Int -> Hs.Expr -> [(Int, QueryType)] -> Hs.Expr
entails _ _ index [] = index
entails _ _ index [(i, Match)] = Hs.call "HashSet.member" [value i, index]
entails _ depth index ((i, Match) : rest) =
  let step = numberedName "step" depth
      lookupValue = Hs.call "HashMap.lookup" [value i, index]
   in Hs.call
        "fromMaybe"
        [ Hs.var "False"
        , Hs.Do
            [Hs.Bind (Hs.PVar step) lookupValue]
            (Hs.call "return" [entails False (depth + 1) (Hs.Var step) rest])
        ]
entails root _ index fields@((_, q) : _) =
  let lhs = Hs.tuplePattern [Hs.PVar (storedName n) | n <- [0 .. length fields - 1]]
      condition =
        Hs.andExpr
          [Hs.apps (Hs.Var (comparisonName kind)) [value i, stored n] | (n, (i, kind)) <- zip [0 ..] fields]
   in case q of
        Meet {} -> Hs.call "any" [Hs.Lambda (lhs :| []) condition, index]
        LatticeMeet {} | root -> Hs.call "fromMaybe" [Hs.var "False", Hs.Do [Hs.Bind lhs index] (Hs.call "return" [condition])]
        LatticeMeet {} -> Hs.Let (Hs.Binding lhs index :| []) condition

insertionCase :: (Text, RelationRepresentationInfo) -> Hs.Decl
insertionCase (rel, layout) =
  Hs.Function
    (Hs.name "insertToDb")
    [Hs.pat "db", factPattern rel (length fields)]
    (Hs.Update (Hs.var "db") [(Hs.name (dbFactSelector rel), updated)])
  where
    fields = storageFields layout
    updated = case fields of
      [] -> Hs.var "True"
      (_, LatticeMeet {}) : _ -> Hs.call "Just" [Hs.tuple (value . fst <$> fields)]
      _ ->
        Hs.Let
          (Hs.Binding (Hs.pat "new_fact") (singletonIndex fields) :| [])
          (Hs.apps (insertionFunction fields) [Hs.var "new_fact", indexExpr rel])

singletonIndex :: [(Int, QueryType)] -> Hs.Expr
singletonIndex [] = Hs.var "True"
singletonIndex fields@((_, Meet {}) : _) = Hs.call "HashSet.singleton" [Hs.tuple (value . fst <$> fields)]
singletonIndex fields@((_, LatticeMeet {}) : _) = Hs.tuple (value . fst <$> fields)
singletonIndex [(i, Match)] = Hs.call "HashSet.singleton" [value i]
singletonIndex ((i, Match) : rest) = Hs.call "HashMap.singleton" [value i, singletonIndex rest]

-- | Insert already merge-complete maximal facts. Preserve the existing
-- component-wise strict-subsumption filter; joins belong in mergeContour.
insertionFunction :: [(Int, QueryType)] -> Hs.Expr
insertionFunction fields@((_, Meet {}) : _) =
  let lhs = Hs.tuplePattern [Hs.PVar (storedName n) | n <- [0 .. length fields - 1]]
      dominated =
        Hs.andExpr $
          concat
            [[Hs.call "/=" [stored n, value i], Hs.apps (Hs.Var (comparisonName q)) [stored n, value i]] | (n, (i, q)) <- zip [0 ..] fields]
      keep = Hs.Lambda (lhs :| []) (Hs.call "not" [dominated])
   in Hs.Lambda
        (Hs.pat "s1" :| [Hs.pat "s2"])
        (Hs.call "HashSet.union" [Hs.var "s1", Hs.call "HashSet.filter" [keep, Hs.var "s2"]])
insertionFunction ((_, LatticeMeet {}) : _) = Hs.var "const"
insertionFunction [(_, Match)] = Hs.var "HashSet.union"
insertionFunction ((_, Match) : rest) = Hs.call "HashMap.unionWith" [insertionFunction rest]
insertionFunction [] = error "Fixen.CodeGen: empty insertion path"

codeGenMergeContour :: RelationRepresentation -> Text
codeGenMergeContour layouts =
  Hs.renderDecls
    (signature "mergeContour" ["Fact", "Database"] (Hs.TyList (Hs.typ "Fact")) : (contourCase <$> Map.toList layouts))

contourCase :: (Text, RelationRepresentationInfo) -> Hs.Decl
contourCase (rel, layout)
  | not (any (isLattice . snd) fields) =
      Hs.Function
        (Hs.name "mergeContour")
        [Hs.PAs (Hs.name "f") (Hs.PCon (Hs.name rel) (replicate arity Hs.PWildcard)), Hs.PWildcard]
        (Hs.List [Hs.var "f"])
  | otherwise =
      let (statements, suffix) = contourSteps True 0 (indexExpr rel) fields
          refinements = zipWith refine [0 ..] suffix
          replacements = IntMap.fromList [(i, Hs.Var (numberedName "joined" i)) | (i, q) <- suffix, isOrdered q]
          result = Hs.call rel [IntMap.findWithDefault (value i) i replacements | i <- [0 .. arity - 1]]
       in Hs.Function
            (Hs.name "mergeContour")
            [Hs.PAs (Hs.name "f") (factPattern rel arity), Hs.pat "db"]
            (Hs.Infix (Hs.var "f") (Hs.name ":") (Hs.Do (statements ++ refinements) (Hs.call "return" [result])))
  where
    fields = storageFields layout
    arity = length fields
    refine n (i, q) = case q of
      Match -> Hs.guardStmt (Hs.call "==" [stored n, value i])
      Meet {} -> Hs.Bind (Hs.PVar (numberedName "joined" i)) (Hs.apps (Hs.Var (lowerName (refinementOperation q))) [stored n, value i])
      LatticeMeet _ join _ -> Hs.LetStmt (Hs.PVar (numberedName "joined" i)) (Hs.apps (Hs.Var (lowerName join)) [stored n, value i])

-- | Collect lookup statements and unpack the terminal ordered suffix.
contourSteps :: Bool -> Int -> Hs.Expr -> [(Int, QueryType)] -> ([Hs.Stmt], [(Int, QueryType)])
contourSteps _ depth index ((i, Match) : rest) =
  let step = numberedName "step" depth
      statement = Hs.Bind (Hs.PVar step) (Hs.call "maybeToList" [Hs.call "HashMap.lookup" [value i, index]])
      (statements, suffix) = contourSteps False (depth + 1) (Hs.Var step) rest
   in (statement : statements, suffix)
contourSteps root _ index fields@((_, q) : _) =
  let lhs = Hs.tuplePattern [Hs.PVar (storedName n) | n <- [0 .. length fields - 1]]
      statement = case q of
        Meet {} -> Hs.Bind lhs (Hs.call "HashSet.toList" [index])
        LatticeMeet {} | root -> Hs.Bind lhs (Hs.call "maybeToList" [index])
        LatticeMeet {} -> Hs.LetStmt lhs index
   in ([statement], fields)
contourSteps _ _ _ [] = error "Fixen.CodeGen: contour without ordered fields"

isLattice :: QueryType -> Bool
isLattice LatticeMeet {} = True
isLattice _ = False

isOrdered :: QueryType -> Bool
isOrdered Match = False
isOrdered _ = True
