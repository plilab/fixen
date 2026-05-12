{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Fixen.Monad.Env.Symbol
-- Description : Symbol information tracking for the Fixen compiler
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module provides facilities for tracking and obtaining information of
-- symbols within a Fixen program.
--
-- @since 0.0.1
module Fixen.Monad.Env.Symbol where

import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict qualified as State
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text
import Fixen.Fields
import Fixen.IR.AST
import Fixen.Monad.Type
import Fixen.Utils
import Prelude hiding (concat, show)

--------------------------------------------------------------------------------

-- * Types

--------------------------------------------------------------------------------

-- | The name of stuff.
--
-- @since 0.0.1
type Name = Text

-- | 'NameMap's map names of stuff ('Name') onto other stuff.
--
-- @since 0.0.1
type NameMap = Map Name

-- | 'NodeMap's map node IDs ('NodeId') onto stuff.
--
-- @since 0.0.1
type NodeMap = IntMap

-- | 'NodeSet's are sets of node IDs ('NodeId').
--
-- @since 0.0.1
type NodeSet = IntSet

-- | The symbol environment.
--
-- @since 0.0.1
data SymbolEnv = SymbolEnv
  { _relationMap :: NameMap RelationInfo
  -- ^ Information about relation symbols
  --
  -- @since 0.0.1
  , _relationParamKindMap :: NameMap Kind
  -- ^ Kind information about the "types" (more accurately, relation parameters)
  --
  -- @since 0.0.1
  , _partialOrdMap :: NameMap PartialOrdDeclaration
  -- ^ Information about partial ord symbols
  --
  -- @since 0.0.1
  , _ruleMap :: NodeMap RuleInfo
  -- ^ Information about rules. Importantly, rules may not be named,
  -- so we use their node IDs to keep track of them.
  --
  -- @since 0.0.1
  , _queryMap :: NameMap Query
  -- ^ Information about query symbols
  --
  -- @since 0.0.1
  , _externMap :: NameMap NodeId
  -- ^ Information about extern symbols. Each name is mapped to the node ID of
  -- its first occurrence.
  --
  -- @since 0.0.1
  , _phaseInfo :: NonEmpty NodeSet
  -- ^ Information about phases of the program.
  --
  -- @since 0.0.1
  , _priorityMap :: NodeMap PriorityInfo
  -- ^ Information about the priority declarations of the program.
  --
  -- @since 0.0.1
  }
  deriving (Show, Eq)

instance HasRelationInfos SymbolEnv (NameMap RelationInfo) where
  relationInfos = lens _relationMap (\s i -> s {_relationMap = i})

instance HasKindInfos SymbolEnv (NameMap Kind) where
  kindInfos = lens _relationParamKindMap (\s i -> s {_relationParamKindMap = i})

instance HasPartialOrdInfos SymbolEnv (NameMap PartialOrdDeclaration) where
  partialOrdInfos = lens _partialOrdMap (\s i -> s {_partialOrdMap = i})

instance HasRuleInfos SymbolEnv (NodeMap RuleInfo) where
  ruleInfos = lens _ruleMap (\s i -> s {_ruleMap = i})

instance HasQueryInfos SymbolEnv (NameMap Query) where
  queryInfos = lens _queryMap (\s i -> s {_queryMap = i})

instance HasExternInfos SymbolEnv (NameMap NodeId) where
  externInfos = lens _externMap (\s i -> s {_externMap = i})

instance HasPhaseInfos SymbolEnv (NonEmpty NodeSet) where
  phaseInfos = lens _phaseInfo (\s i -> s {_phaseInfo = i})

instance HasPriorityInfos SymbolEnv (NodeMap PriorityInfo) where
  priorityInfos = lens _priorityMap (\s i -> s {_priorityMap = i})

-- | Information about relations in the program.
--
-- @since 0.0.1
data RelationInfo = RelationInfo
  { _relationDeclaration :: RelationDeclaration
  -- ^ The 'RelationDeclaration'
  --
  -- @since 0.0.1
  , _relationArgMatchInfo :: [RelationArgMatchInfo]
  -- ^ Information about whether relation arguments are ever matched in rules.
  -- The \(i^\text{th}\) element corresponds to the matching information for
  -- the \(i^\text{th}\) argument to the relation.
  --
  -- @since 0.0.1
  }
  deriving (Show, Eq)

instance HasDeclaration RelationInfo RelationDeclaration where
  declaration = lens _relationDeclaration (\s i -> s {_relationDeclaration = i})

instance HasMatchInfos RelationInfo [RelationArgMatchInfo] where
  matchInfos = lens _relationArgMatchInfo (\s i -> s {_relationArgMatchInfo = i})

-- | Information about whether a relation argument is matched in rules.
--
-- @since 0.0.1
data RelationArgMatchInfo = Unmatched | Matched
  deriving
    (Show, Eq)

-- | Kind information about relation argument types.
--
-- @since 0.0.1
data Kind = Discrete | PartiallyOrdered
  deriving (Show, Eq)

-- | Information about rules.
--
-- @since 0.0.1
data RuleInfo = RuleInfo
  { _ruleDeclaration :: Rule
  -- ^ The 'Rule' declaration
  --
  -- @since 0.0.1
  , _ruleBoundVars :: NameMap RuleParameterInfo
  -- ^ Information about the rule's parameters
  --
  -- @since 0.0.1
  }
  deriving (Show, Eq)

instance HasDeclaration RuleInfo Rule where
  declaration = lens _ruleDeclaration (\s i -> s {_ruleDeclaration = i})

instance HasArgs RuleInfo (NameMap RuleParameterInfo) where
  args = lens _ruleBoundVars (\s i -> s {_ruleBoundVars = i})

-- | Information about rule parameters
--
-- @since 0.0.1
data RuleParameterInfo = RuleParameterInfo
  { _ruleParamType :: TypeLattice
  , _ruleParamUsage :: [UsageInfo]
  -- ^ Where it is declared (rule/priority representative).
  --
  -- @since 0.0.1
  , _ruleParamVar :: SimpleIdentifier
  -- ^ Where the variable appears. It could be one of the arguments in an
  -- assumption (if no explicit local vars were provided), or one of the
  -- variables explicitly specified as a rule local variable
  --
  -- @since 0.0.1
  }
  deriving (Show, Eq)

instance HasType RuleParameterInfo TypeLattice where
  ty = lens _ruleParamType (\s i -> s {_ruleParamType = i})

instance HasUsageInfo RuleParameterInfo [UsageInfo] where
  usageInfo = lens _ruleParamUsage (\s i -> s {_ruleParamUsage = i})

instance HasVar RuleParameterInfo SimpleIdentifier where
  var = lens _ruleParamVar (\s i -> s {_ruleParamVar = i})

-- | Information about priority declarations.
--
-- @since 0.0.1
data PriorityInfo = PriorityInfo
  { _priorityDeclaration :: Priority
  -- ^ The 'Priority' declaration
  --
  -- @since 0.0.1
  , _priorityLocalVars :: NameMap (NodeId, SimpleIdentifier)
  -- ^ The keys are the local variables themselves, the values are the rule
  -- parameters it is attached to and the var itself.
  --
  -- @since 0.0.1
  , _priorityRules :: (NodeId, NodeId)
  -- ^ The 'NodeId's of the rule instances in this priority declaration. The
  -- first projection is the priority's LHS, and the second projection is the
  -- rule's RHS.
  --
  -- @since 0.0.1
  }
  deriving (Show, Eq)

instance HasDeclaration PriorityInfo Priority where
  declaration = lens _priorityDeclaration (\s i -> s {_priorityDeclaration = i})

instance HasArgs PriorityInfo (NameMap (NodeId, SimpleIdentifier)) where
  args = lens _priorityLocalVars (\s i -> s {_priorityLocalVars = i})

instance HasRules PriorityInfo (NodeId, NodeId) where
  rules = lens _priorityRules (\s i -> s {_priorityRules = i})

instance HasLHS PriorityInfo NodeId where
  lhs =
    lens
      (fst ∘ _priorityRules)
      (\s i -> s {_priorityRules = (i, snd (s ^. rules))})

instance HasRHS PriorityInfo NodeId where
  rhs =
    lens
      (snd ∘ _priorityRules)
      (\s i -> s {_priorityRules = (fst (s ^. rules), i)})

-- | Information about types during type checking.
--
-- @since 0.0.1
data TypeLattice
  = -- | Not sure what the type is, particularly due to Haskell expressions
    --
    -- @since 0.0.1
    Dynamic
  | -- | A type known by Fixen
    --
    -- @since 0.0.1
    ActualType
      Type
      -- ^ The actual 'Type'
      --
      -- @since 0.0.1
      TypeEvidence
      -- ^ First discovered usage that gives us information about why the type
      -- is as such
      --
      -- @since 0.0.1
  | -- | Ill-typed.
    --
    -- @since 0.0.1
    Bottom
  deriving (Show, Eq)

-- | Evidence of a term having a type.
--
-- @since 0.0.1
data TypeEvidence
  = -- | Type was ascribed because it was used in an assumption
    --
    -- @since 0.0.1
    TypedViaAssumption
      Int
      -- ^ The ith assumption of the rule
      --
      -- @since 0.0.1
      Int
      -- ^ The jth argument to the assumption
      --
      -- @since 0.0.1
  | -- | Type was ascribed because it was used in the conclusion
    --
    -- @since 0.0.1
    TypedViaConclusion
      Int
      -- ^ The ith argument to the conclusion
      --
      -- @since 0.0.1
  deriving (Show, Eq)

-- | Information about the usage of a rule parameter
--
-- @since 0.0.1
data UsageInfo
  = -- | The rule parameter is used in an assumtion
    --
    -- @since 0.0.1
    UsedInAssumption
      Int
      -- ^ The ith assumption of the rule
      --
      -- @since 0.0.1
      Int
      -- ^ The jth argument to the assumption
      --
      -- @since 0.0.1
  | -- | Used in one of the rule's conditions. At the moment, it is not
    -- necessary track which condition a variable is used in
    --
    -- @since 0.0.1
    UsedInCondition
  | -- | Used somewhere in the rule's conclusion. At the moment, it is not
    -- necessary to track where in the conclusion is the variable used
    --
    -- @since 0.0.1
    UsedInConclusion
  deriving (Show, Eq)

-- | The types of states that contain a 'SymbolEnv'.
--
-- @since 0.0.1
type Symboled σ = σ :>: SymbolEnv

isUsedInAssumption :: UsageInfo -> Bool
isUsedInAssumption (UsedInAssumption _ _) = True
isUsedInAssumption _ = False

calculateRepresentativeFromType :: Type -> Name
calculateRepresentativeFromType (TypeName _ i) = fullIdentifier i
calculateRepresentativeFromType (TypeApp _ l r) =
  concat
    [ "("
    , calculateRepresentativeFromType l
    , " "
    , calculateRepresentativeFromType r
    , ")"
    ]
calculateRepresentativeFromType (TypeList _ l) =
  concat
    [ "["
    , calculateRepresentativeFromType l
    , "]"
    ]
calculateRepresentativeFromType (TypeUnit _) = "()"
calculateRepresentativeFromType (TypeNatLit _ i) = show i
calculateRepresentativeFromType (TypeSymbolLit _ i) = show i
calculateRepresentativeFromType (TypeTuple _ hd tl) =
  let ls = hd : Data.List.NonEmpty.toList tl
   in concat
        [ "("
        , intercalate ", " (calculateRepresentativeFromType <$> ls)
        , ")"
        ]

emptySymbolEnv :: SymbolEnv
emptySymbolEnv =
  SymbolEnv
    { _relationMap = Map.empty
    , _relationParamKindMap = Map.empty
    , _partialOrdMap = Map.empty
    , _ruleMap = IntMap.empty
    , _queryMap = Map.empty
    , _externMap = Map.empty
    , _phaseInfo = IntSet.empty :| []
    , _priorityMap = IntMap.empty
    }

fixenGetSymbolEnv :: Symboled a => FixenPass a (SymbolEnv)
fixenGetSymbolEnv = do
  st <- State.get
  let env :: SymbolEnv = (↓) st
  return env

fixenGetRelationInfo :: Symboled a => FixenPass a (NameMap RelationInfo)
fixenGetRelationInfo = do
  env <- fixenGetSymbolEnv
  return $ env ^. relationInfos

fixenGetPhases :: Symboled a => FixenPass a (NonEmpty NodeSet)
fixenGetPhases = do
  st <- State.get
  let env :: SymbolEnv = (↓) st
  return $ env ^. phaseInfos

fixenGetPartialOrdInfo :: Symboled a => FixenPass a (NameMap PartialOrdDeclaration)
fixenGetPartialOrdInfo = do
  env <- fixenGetSymbolEnv
  return $ env ^. partialOrdInfos

fixenGetRelationParamKindInfo :: Symboled a => FixenPass a (NameMap Kind)
fixenGetRelationParamKindInfo = do
  env <- fixenGetSymbolEnv
  return $ env ^. kindInfos

fixenGetRelationParamKind :: Symboled a => Type -> FixenPass a Kind
fixenGetRelationParamKind t = do
  let n = calculateRepresentativeFromType t
  fixenGetRelationParamKindFromName n

fixenGetRelationParamKindFromName :: Symboled a => Text -> FixenPass a Kind
fixenGetRelationParamKindFromName n = do
  info <- fixenGetRelationParamKindInfo
  return $ info Map.! n

fixenGetRuleInfo :: Symboled a => FixenPass a (NodeMap RuleInfo)
fixenGetRuleInfo = do
  env <- fixenGetSymbolEnv
  return $ env ^. ruleInfos

getUnderlyingType :: Symboled a => Type -> FixenPass a Type
getUnderlyingType t = do
  let n = calculateRepresentativeFromType t
  p_ord <- fixenGetPartialOrdInfo
  case p_ord ^. at n of
    Nothing -> return t
    Just p_ord_dec -> return $ partialOrdDeclarationType p_ord_dec

fixenGetPriorities :: Symboled a => FixenPass a (NodeMap PriorityInfo)
fixenGetPriorities = do
  env <- fixenGetSymbolEnv
  return $ env ^. priorityInfos
