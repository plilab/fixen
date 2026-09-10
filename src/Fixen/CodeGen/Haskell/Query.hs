{-# LANGUAGE OverloadedStrings #-}

-- | Query functions over inferred relation indexes.
module Fixen.CodeGen.Haskell.Query (codeGenQuery) where

import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Fixen.CodeGen.Common
import Fixen.CodeGen.Haskell.Bindings
import Fixen.CodeGen.Haskell.Syntax qualified as Hs
import Fixen.IR.AST
import Fixen.IR.RelationRepresentation
import Fixen.Monad

codeGenQuery :: RelationRepresentation -> Query -> FixenPass CodeGenState Text
codeGenQuery layouts query = do
  phases <- fixenGetPhases
  let rel = queryRel query
      relName = simpleIdentifier (relationLikeName rel)
      queryName' = Hs.name (simpleIdentifier (queryName query))
      layout = layouts Map.! relName
      fact = _factRepresentation layout
      database = _databaseRepresentation layout
      inputs = [(t, i) | ((_, t), Input _, i) <- zip3 (_factTypes fact) (relationLikeArgs rel) [0 ..]]
      inputIds = [(_insertionMap fact IntMap.! i) | (_, i) <- inputs]
      bindings = inputBindings inputIds
      singlePhase = length phases == 1
      extraTypes = if singlePhase then [Hs.typ "Database"] else [Hs.typ "Interpretation", Hs.typ "Phase"]
      extraArgs = if singlePhase then [Hs.pat "db"] else [Hs.pat "i", Hs.pat "p"]
      initial = [Hs.LetStmt (Hs.pat "db") (Hs.call "selectDb" [Hs.var "i", Hs.var "p"]) | not singlePhase]
      fields = [(i, q, storage) | (i, (q, storage, _)) <- zip [0 ..] (_databaseTypes database)]
      index = Hs.call (dbFactSelector relName) [Hs.var "db"]
      (statements, final) = querySteps True bindings index fields
      result = Hs.call relName [Hs.Var (boundVariable i final) | i <- IntMap.elems (_insertionMap fact)]
  pure $
    Hs.renderDecls
      [ Hs.Signature queryName' (Hs.functionType ((lowerType . fst <$> inputs) ++ extraTypes) (Hs.TyList (Hs.typ "Fact")))
      , Hs.Function
          queryName'
          (((\i -> Hs.PVar (boundVariable i bindings)) <$> inputIds) ++ extraArgs)
          (Hs.Do (initial ++ statements) (Hs.call "return" [result]))
      ]

querySteps :: Bool -> Bindings -> Hs.Expr -> [(Int, QueryType, StoreType)] -> ([Hs.Stmt], Bindings)
querySteps _ bindings index [] = ([Hs.guardStmt index], bindings)
querySteps _ bindings index ((i, Match, StoredAsHashMap) : fields) =
  let (step, withStep) = freshTemporary bindings
      (statement, next) = case lookupVariable i bindings of
        Nothing -> let (n, withVar) = bindVariable i withStep in (Hs.Bind (Hs.PTuple [Hs.PVar n, Hs.PVar step]) (Hs.call "HashMap.toList" [index]), withVar)
        Just n -> (Hs.Bind (Hs.PVar step) (Hs.call "maybeToList" [Hs.call "HashMap.lookup" [Hs.Var n, index]]), withStep)
      (rest, final) = querySteps False next (Hs.Var step) fields
   in (statement : rest, final)
querySteps _ bindings index [(i, Match, StoredAsHashSet)] = case lookupVariable i bindings of
  Nothing -> let (n, next) = bindVariable i bindings in ([Hs.Bind (Hs.PVar n) (Hs.call "HashSet.toList" [index])], next)
  Just n -> ([Hs.guardStmt (Hs.call "HashSet.member" [Hs.Var n, index])], bindings)
querySteps root bindings index fields@((_, q, _) : _) =
  let (patterns, guards, final) = orderedFields bindings fields
      lhs = Hs.tuplePattern patterns
      extract = case q of
        Meet {} -> Hs.Bind lhs (Hs.call "HashSet.toList" [index])
        LatticeMeet {} | root -> Hs.Bind lhs (Hs.call "maybeToList" [index])
        LatticeMeet {} -> Hs.LetStmt lhs index
        Match -> error "Fixen.CodeGen: invalid query index suffix"
   in (extract : guards, final)

-- | Query inputs constrain stored values by leq; they do not compute meets.
orderedFields :: Bindings -> [(Int, QueryType, StoreType)] -> ([Hs.Pattern], [Hs.Stmt], Bindings)
orderedFields bindings [] = ([], [], bindings)
orderedFields bindings ((i, q, _) : fields) =
  let (n, next) = bindVariable i bindings
      guards = case lookupVariable i bindings of
        Nothing -> []
        Just old -> [Hs.guardStmt (Hs.apps (Hs.Var (leqName q)) [Hs.Var old, Hs.Var n])]
      (patterns, rest, final) = orderedFields next fields
   in (Hs.PVar n : patterns, guards ++ rest, final)
  where
    leqName (Meet leq _) = lowerName leq
    leqName (LatticeMeet leq _ _) = lowerName leq
    leqName Match = error "Fixen.CodeGen: discrete query field after ordered field"
