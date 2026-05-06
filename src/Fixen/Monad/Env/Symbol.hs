{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Fixen.Data.NodeId
import Fixen.IR.AST
import Fixen.Monad.Type
import Prelude hiding (concat, show)

-- | The name of stuff.
type Name = Text

-- | 'NameMap's map names of stuff ('Name') onto other stuff.
type NameMap = Map Name

-- | 'NodeMap's map node IDs ('NodeId') onto stuff.
type NodeMap = IntMap

-- | 'NodeSet's are sets of node IDs ('NodeId').
type NodeSet = IntSet

data SymbolEnv = SymbolEnv
  { _relationMap :: NameMap RelationInfo
  -- ^ Information about relation symbols
  , _relationParamKindMap :: NameMap Kind
  -- ^ Kind information about the "types" (more accurately, relation parameters)
  , _partialOrdMap :: NameMap PartialOrdDeclaration
  -- ^ Information about partial ord symbols
  , _ruleMap :: NodeMap RuleInfo
  -- ^ Information about rules. Importantly, rules may not be named,
  -- so we use their node IDs to keep track of them.
  , _queryMap :: NameMap Query
  -- ^ Information about query symbols
  , _externMap :: NameMap NodeId
  -- ^ Information about extern symbols. Each name is mapped to the node ID of
  -- its first occurrence.
  , _phaseInfo :: NonEmpty NodeSet
  -- ^ Information about phases of the program.
  , _priorityMap :: NodeMap PriorityInfo
  }
  deriving (Show, Eq)

data RelationInfo = RelationInfo
  { _relationDeclaration :: Relation
  , _relationArgMatchInfo :: [RelationArgMatchInfo]
  }
  deriving (Show, Eq)

data RelationArgMatchInfo = Unmatched | Matched
  deriving
    (Show, Eq)

data Kind = Discrete | PartiallyOrdered
  deriving (Show, Eq)

data RuleInfo = RuleInfo
  { _ruleDeclaration :: Rule
  , _ruleBoundVars :: NameMap LocalVarInfo
  }
  deriving (Show, Eq)

data PriorityInfo = PriorityInfo
  { _priorityDeclaration :: Priority
  , _priorityLocalVars :: NameMap (NodeId, SimpleIdentifier)
  -- ^ The keys are the local variables themselves, the values are the rule
  -- parameters it is attached to and the var itself.
  , _priorityRules :: (Int, Int)
  }
  deriving (Show, Eq)

data LocalVarInfo = LocalVarInfo
  { _localVarType :: TypeLattice
  , _localVarUsage :: [UsageInfo]
  -- ^ Where it is declared (rule/priority representative).
  , _localVarVar :: SimpleIdentifier
  -- ^ Where the variable appears. It could be one of the arguments in an
  -- assumption (if no explicit local vars were provided), or one of the
  -- variables explicitly specified as a rule local variable
  }
  deriving (Show, Eq)

data TypeLattice
  = Dynamic
  | ActualType
      Type
      -- ^ The actual 'Type'
      TypeEvidence
      -- ^ First discovered usage that gives us information about why the type
      -- is as such
  | Bottom
  deriving (Show, Eq)

data TypeEvidence
  = TypedViaAssumption Int Int
  | TypedViaConclusion Int
  deriving (Show, Eq)

data UsageInfo
  = UsedInAssumption Int Int
  | UsedInCondition
  | UsedInConclusion
  deriving (Show, Eq)

type Symboled σ = σ :>: SymbolEnv

isUsedInAssumption :: UsageInfo -> Bool
isUsedInAssumption (UsedInAssumption _ _) = True
isUsedInAssumption _ = False

-- Lenses

makeLenses ''SymbolEnv
makeLenses ''RelationInfo
makeLenses ''RuleInfo
makeLenses ''LocalVarInfo
makeLenses ''PriorityInfo

calculateRepresentativeFromType :: Type -> Name
calculateRepresentativeFromType (TypeName _ i) = fullIdentifier i
calculateRepresentativeFromType (TypeApp _ lhs rhs) =
  concat
    [ "("
    , calculateRepresentativeFromType lhs
    , " "
    , calculateRepresentativeFromType rhs
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
  return $ env ^. relationMap

fixenGetPhases :: Symboled a => FixenPass a (NonEmpty NodeSet)
fixenGetPhases = do
  st <- State.get
  let env :: SymbolEnv = (↓) st
  return $ env ^. phaseInfo

fixenGetPartialOrdInfo :: Symboled a => FixenPass a (NameMap PartialOrdDeclaration)
fixenGetPartialOrdInfo = do
  env <- fixenGetSymbolEnv
  return $ env ^. partialOrdMap

fixenGetRelationParamKindInfo :: Symboled a => FixenPass a (NameMap Kind)
fixenGetRelationParamKindInfo = do
  env <- fixenGetSymbolEnv
  return $ env ^. relationParamKindMap

fixenGetRelationParamKind :: Symboled a => Type -> FixenPass a Kind
fixenGetRelationParamKind ty = do
  let n = calculateRepresentativeFromType ty
  fixenGetRelationParamKindFromName n

fixenGetRelationParamKindFromName :: Symboled a => Text -> FixenPass a Kind
fixenGetRelationParamKindFromName n = do
  info <- fixenGetRelationParamKindInfo
  return $ info Map.! n

fixenGetRuleInfo :: Symboled a => FixenPass a (NodeMap RuleInfo)
fixenGetRuleInfo = do
  env <- fixenGetSymbolEnv
  return $ env ^. ruleMap

getUnderlyingType :: Symboled a => Type -> FixenPass a Type
getUnderlyingType t = do
  let name = calculateRepresentativeFromType t
  p_ord <- fixenGetPartialOrdInfo
  case p_ord ^. at name of
    Nothing -> return t
    Just p_ord_dec -> return $ partialOrdDeclarationType p_ord_dec

fixenGetPriorities :: Symboled a => FixenPass a (NodeMap PriorityInfo)
fixenGetPriorities = do
  env <- fixenGetSymbolEnv
  return $ env ^. priorityMap
