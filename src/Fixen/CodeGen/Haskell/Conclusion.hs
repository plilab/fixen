{-# LANGUAGE OverloadedStrings #-}

-- | First-match selection of a batch, with hygienic case-local names.
module Fixen.CodeGen.Haskell.Conclusion (lowerConclusionBody) where

import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Fixen.CodeGen.Common
import Fixen.CodeGen.Haskell.Pattern (lowerPattern)
import Fixen.CodeGen.Haskell.Syntax qualified as Hs
import Fixen.IR.AST
import Fixen.SymbolSolver.Common (getAllExprNames)

lowerConclusionBody :: ConclusionBody -> Hs.Expr
lowerConclusionBody root = go Map.empty root
  where
    sourceNames = Set.fromList [simpleIdentifier v | (_, e) <- conclusionExpressions root, v <- Set.toList (getAllExprNames e)]
    fresh v = Hs.name (avoid ("_fixen_case" <> Text.show (simpleIdentifierNodeId v)))
    avoid n
      | Set.member n sourceNames = avoid (n <> "'")
      | otherwise = n
    go names (Emit cs) = Hs.List [Hs.call (simpleIdentifier (relationLikeName c)) (lowerExprWithNames names <$> relationLikeArgs c) | c <- NE.toList cs]
    go names (GuardedConclusions arms) = foldr select (Hs.List []) arms
      where
        select (Nothing, body) _ = go names body
        select (Just guard, body) rest = Hs.If (lowerExprWithNames names guard) (go names body) rest
    go names (CaseConclusions value arms) =
      let lowerArm (p, body) =
            let locals = Map.fromList [(simpleIdentifier v, fresh v) | v <- patternVariables p]
                pat = runIdentity (lowerPattern (pure . Hs.PVar . (locals Map.!) . simpleIdentifier) p)
             in (pat, go (Map.union locals names) body)
          fallback = if any (\(p, _) -> case p of PatternVar _ -> True; _ -> False) arms then [] else [(Hs.PWildcard, Hs.List [])]
          first :| rest = fmap lowerArm arms
       in Hs.Case (lowerExprWithNames names value) (first :| (rest ++ fallback))
