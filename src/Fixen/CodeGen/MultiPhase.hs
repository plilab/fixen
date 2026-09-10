{-# LANGUAGE OverloadedStrings #-}

-- | Supporting declarations for programs with more than one phase.
module Fixen.CodeGen.MultiPhase (codeGenMultiPhase) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Fixen.CodeGen.Common
import Fixen.CodeGen.Haskell.Bindings (numberedName)
import Fixen.CodeGen.Haskell.Syntax qualified as Hs
import Fixen.Monad

codeGenMultiPhase :: FixenPass CodeGenState Text
codeGenMultiPhase = do
  count <- NonEmpty.length <$> fixenGetPhases
  pure $
    if count == 1
      then ""
      else
        Hs.renderDecls (phaseDeclarations count) <> "\n\ninfix 1 ||=\n"

phaseDeclarations :: Int -> [Hs.Decl]
phaseDeclarations count =
  [ Hs.TypeAlias (Hs.name "Interpretation") (Hs.TyTuple (replicate count (Hs.typ "Database")))
  , sig "emptyInterpretation" [] "Interpretation"
  , fn "emptyInterpretation" [] (Hs.Tuple (replicate count (Hs.var "emptyDb")))
  , Hs.Data
      (Hs.name "Phase")
      (NonEmpty.fromList [Hs.Constructor (phase n) [] | n <- indices])
      (Hs.name <$> ["Eq", "Show", "Ord"])
  , sig "nextPhase" ["Phase"] "Phase"
  ]
    ++ [Hs.Function (Hs.name "nextPhase") [phasePat n] (Hs.Var (phase ((n + 1) `mod` count))) | n <- indices]
    ++ [sig "selectDb" ["Interpretation", "Phase"] "Database"]
    ++ [ Hs.Function
           (Hs.name "selectDb")
           [Hs.PTuple [if n == i then Hs.pat "db" else Hs.PWildcard | i <- indices], phasePat n]
           (Hs.var "db")
       | n <- indices
       ]
    ++ [ sig "||=" ["Interpretation", "Fact", "Phase"] "Bool"
       , fn "||=" ["i", "f", "p"] (Hs.call "|=" [Hs.call "selectDb" [Hs.var "i", Hs.var "p"], Hs.var "f"])
       , sig "replaceDb" ["Interpretation", "Database", "Phase"] "Interpretation"
       ]
    ++ [ Hs.Function
           (Hs.name "replaceDb")
           [Hs.PTuple [if n == i then Hs.PWildcard else Hs.PVar (db i) | i <- indices], Hs.pat "db'", phasePat n]
           (Hs.Tuple [if n == i then Hs.var "db'" else Hs.Var (db i) | i <- indices])
       | n <- indices
       ]
    ++ [ sig "insertToInterpretation" ["Interpretation", "Fact", "Phase"] "Interpretation"
       , fn
           "insertToInterpretation"
           ["i", "f", "p"]
           ( Hs.Let
               ( Hs.Binding (Hs.pat "db") (Hs.call "selectDb" [Hs.var "i", Hs.var "p"])
                   :| [Hs.Binding (Hs.pat "db'") (Hs.call "insertToDb" [Hs.var "db", Hs.var "f"])]
               )
               (Hs.call "replaceDb" [Hs.var "i", Hs.var "db'", Hs.var "p"])
           )
       , Hs.Signature
           (Hs.name "evaluatePhased")
           (Hs.TyArrow (Hs.TyTuple [Hs.typ "RuleInstance", Hs.typ "Phase"]) (Hs.TyTuple [Hs.typ "Fact", Hs.typ "Phase"]))
       , Hs.Function
           (Hs.name "evaluatePhased")
           [Hs.PTuple [Hs.pat "r", Hs.pat "p"]]
           (Hs.Tuple [Hs.call "evaluate" [Hs.var "r"], Hs.call "nextPhase" [Hs.var "p"]])
       ]
  where
    indices = [0 .. count - 1]
    phase = numberedName "Phase"
    phasePat n = Hs.PCon (phase n) []
    db = numberedName "db"
    sig n args result = Hs.Signature (Hs.name n) (Hs.functionType (Hs.typ <$> args) (Hs.typ result))
    fn n args = Hs.Function (Hs.name n) (Hs.pat <$> args)
