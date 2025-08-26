{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Mozzarella.IR.ExplicitBoundVars where

import Data.Text
import Data.Void
import Error.Diagnose.Position
import Mozzarella.Data.AlaCarte
import Mozzarella.IR.AST qualified as AST
import Mozzarella.IR.Core

type Rule =
  CoreRule
    "ExplicitBoundVars.Rule"
    Position
    (Maybe AST.TermLetterIdentifier)
    RuleBoundVars
    AST.RulePremises
    AST.Conclusion
pattern Rule
  :: Position
  -- ^ The annotation
  -> Maybe AST.TermLetterIdentifier
  -- ^ The name of the rule
  -> RuleBoundVars
  -- ^ The bound variables of the rule
  -> AST.RulePremises
  -- ^ The premises of the rule
  -> AST.Conclusion
  -- ^ The conclusion of the rule
  -> Rule
pattern Rule pos name bvs prems concl = CoreRule pos name bvs prems concl
{-# COMPLETE Rule #-}

type RuleBoundVars = CoreItem "ExplicitBoundVars.Rule#bound_vars" PositionOrGenerated [BoundVar] Void
pattern RuleBoundVars
  :: PositionOrGenerated
  -- ^ The annotation
  -> [BoundVar]
  -- ^ The identifiers
  -> RuleBoundVars
pattern RuleBoundVars pos ls = CoreItem pos ls

type BoundVar =
  CoreItem
    "Explicit.BoundVar"
    BoundVarSource
    Text
    Void

pattern BoundVar
  :: BoundVarSource
  -- ^ The annotation
  -> Text
  -- ^ The identifier itself
  -> BoundVar
pattern BoundVar a b = CoreItem a b
{-# COMPLETE BoundVar #-}

-- data BoundVarSource
--   = ExplicitBoundVar
--   | Inferred [(Int, Int)]
--   deriving (Show, Eq)

data Program = Program
  { externs :: Maybe AST.Extern
  , relations :: [AST.Relation]
  , rules :: [Rule]
  }
  deriving (Show, Eq)

data PositionOrGenerated = ActuallyPosition Position | Generated deriving (Show, Eq)

data BoundVarSource = SourcePosition Position | Inferred [(Int, Int)] deriving (Show, Eq)
