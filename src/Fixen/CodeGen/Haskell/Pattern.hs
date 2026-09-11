{-# LANGUAGE OverloadedStrings #-}

-- | Pattern matching at activation and reconstruction from retained arguments.
-- Captured values never need fields (or type annotations) of their own.
module Fixen.CodeGen.Haskell.Pattern (matchRulePatterns, storedRulePatterns) where

import Control.Monad.State.Strict
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Fixen.CodeGen.Common (lowerName)
import Fixen.CodeGen.Haskell.Syntax qualified as Hs
import Fixen.IR.AST

lowerPattern :: Monad m => (SimpleIdentifier -> m Hs.Pattern) -> Pattern -> m Hs.Pattern
lowerPattern variable p = case p of
  PatternVar v | simpleIdentifier v == "_" -> pure Hs.PWildcard
  PatternVar v -> variable v
  PatternCon _ c ps -> Hs.PCon (lowerName c) <$> mapM (lowerPattern variable) ps
  PatternTuple _ ps -> Hs.PTuple <$> mapM (lowerPattern variable) ps
  PatternList _ ps -> Hs.PList <$> mapM (lowerPattern variable) ps
  PatternInt _ n -> pure (Hs.PInteger n)
  PatternString _ s -> pure (Hs.PString s)
  PatternChar _ c -> pure (Hs.PChar c)

-- | Refutable list binds skip mismatches. Every occurrence gets a fresh binder;
-- repeats become equality guards, including repeats within a single pattern.
matchRulePatterns :: NodeId -> [(SimpleIdentifier, Pattern)] -> Map.Map Text Hs.Name -> ([Hs.Stmt], Map.Map Text Hs.Name)
matchRulePatterns ruleId patterns initial =
  let (statements, (_, names, _)) = runState (concat <$> mapM match patterns) (0 :: Int, initial, [])
   in (statements, names)
  where
    match (whole, p) = do
      modify (\(i, ns, _) -> (i, ns, []))
      pat <- lowerPattern bind p
      (_, names, guards) <- get
      pure (Hs.Bind pat (Hs.List [Hs.Var (names Map.! simpleIdentifier whole)]) : reverse guards)
    bind v = do
      (i, names, guards) <- get
      let source = simpleIdentifier v
          fresh = Hs.name ("_fixen_capture" <> Text.show ruleId <> "_" <> Text.show i)
          (nextNames, nextGuards) = case Map.lookup source names of
            Nothing -> (Map.insert source fresh names, guards)
            Just old -> (names, Hs.guardStmt (Hs.call "==" [Hs.Var old, Hs.Var fresh]) : guards)
      put (i + 1, nextNames, nextGuards)
      pure (Hs.PVar fresh)

-- | Reconstruct only requested variables. Equality and constructor checks have
-- already succeeded before enqueueing; repeated binders can be wildcards here.
-- Used for evaluation and for priority mappings, including capture-only vars.
storedRulePatterns :: Rule -> Map.Map Text Hs.Name -> [Hs.Pattern]
storedRulePatterns r requested = evalState (mapM field (Map.toAscList (ruleStorageVariables r))) requested
  where
    patterns = Map.fromList [(simpleIdentifier v, p) | (v, p) <- ruleDestructuring r]
    -- Match-time bindings prefer an existing whole-variable binding. Otherwise
    -- the first capture in source order wins. Preserve that representative here
    -- too: user-defined Eq need not make the values observationally identical.
    owners =
      Map.union
        (Map.mapWithKey (\n _ -> n) (ruleStorageVariables r))
        (Map.fromList [(simpleIdentifier v, simpleIdentifier whole) | (whole, p) <- reverse (ruleDestructuring r), v <- patternVariables p])
    field (source, v) = case Map.lookup source patterns of
      Nothing -> bind v
      Just p -> do
        remaining <- get
        if any (\x -> Map.member (simpleIdentifier x) remaining && owns source x) (patternVariables p)
          then lowerPattern (\x -> if owns source x then bind x else pure Hs.PWildcard) p
          else pure Hs.PWildcard
    owns source v = Map.lookup (simpleIdentifier v) owners == Just source
    bind v = do
      remaining <- get
      let source = simpleIdentifier v
      modify (Map.delete source)
      pure (maybe Hs.PWildcard Hs.PVar (Map.lookup source remaining))
