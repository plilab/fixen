{-# LANGUAGE OverloadedStrings #-}

module Fixen.Monad.Env.Symbol where

import Control.Applicative
import Control.Lens
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text
import Fixen.Data.NodeId
import Fixen.IR.AST
import Fixen.Monad.Type
import Prelude hiding (concat, show)

-- everything can be mapped to some string that uniquely identifies it
-- the string is the node's representative.
-- Every node has an ID that maps to the representative.
-- Every representative has attached information.
-- Representatives are internal; they should not need to be used by users.

data SymbolEnv = SymbolEnv
  { _infoMap :: InfoMap
  -- ^ Information for a representative
  , _nodeMap :: NodeMap
  -- ^ Maps nodes to representatives
  }
  deriving (Show, Eq)

infoMap :: Lens' SymbolEnv InfoMap
infoMap = lens _infoMap (\env i -> env {_infoMap = i})

nodeMap :: Lens' SymbolEnv NodeMap
nodeMap = lens _nodeMap (\env i -> env {_nodeMap = i})

type Representative = Text
type RepresentativeMap = Map Representative
type NodeMap = IntMap Representative

data InfoMap = InfoMap
  { _relationInfoMap :: RepresentativeMap RelationInfo
  -- ^ Information about 'Relation's. Keys are relation names.
  -- Values store:
  -- 1. The 'Relation' declaration,
  -- 2. Information about whether relation argument positions are ever
  --    matched in any rule.
  , _relationParamKindInfoMap :: RepresentativeMap Kind
  -- ^ "Relation parameters" are types that appear in relation declarations.
  -- This stores their 'Kind' information, i.e., if they are discrete or
  -- partially ordered. Here, keys are type representatives.
  , _partialOrdInfoMap :: RepresentativeMap PartialOrdDeclaration
  -- ^ Information about 'PartialOrdDeclaration's.
  , _ruleInfoMap :: RepresentativeMap RuleInfo
  -- ^ Information about 'Rule's
  , _queryInfoMap :: RepresentativeMap Query
  , _externInfoMap :: RepresentativeMap NodeId
  -- ^ We need this to obtain clashes between names and those that will likely
  -- appear in the source code. Each key is a name, and the value is just one
  -- occurrence in the program; this is enough to throw an error to the user.
  -- these do not ever have fully qualified names.
  --
  -- Anyway, this is only a best effort at finding external names. It is up
  -- to the user to write Fixen programs properly to obtain compilable Haskell
  -- source.
  }
  deriving (Show, Eq)

relationInfoMap :: Lens' InfoMap (RepresentativeMap RelationInfo)
relationInfoMap = lens _relationInfoMap (\im i -> im {_relationInfoMap = i})

relationParamKindInfoMap :: Lens' InfoMap (RepresentativeMap Kind)
relationParamKindInfoMap = lens _relationParamKindInfoMap (\im i -> im {_relationParamKindInfoMap = i})

partialOrdInfoMap :: Lens' InfoMap (RepresentativeMap PartialOrdDeclaration)
partialOrdInfoMap = lens _partialOrdInfoMap (\im i -> im {_partialOrdInfoMap = i})

ruleInfoMap :: Lens' InfoMap (RepresentativeMap RuleInfo)
ruleInfoMap = lens _ruleInfoMap (\im i -> im {_ruleInfoMap = i})

queryInfoMap :: Lens' InfoMap (RepresentativeMap Query)
queryInfoMap = lens _queryInfoMap (\im i -> im {_queryInfoMap = i})

externInfoMap :: Lens' InfoMap (RepresentativeMap NodeId)
externInfoMap = lens _externInfoMap (\im i -> im {_externInfoMap = i})

calculateRepresentativeFromType :: Type -> Representative
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

insertNode :: NodeId -> Representative -> SymbolEnv -> SymbolEnv
insertNode node_id symb_id env =
  env
    & nodeMap
      . at node_id
      ?~ symb_id

insertExternInfos :: RepresentativeMap NodeId -> SymbolEnv -> SymbolEnv
insertExternInfos mp env =
  env
    & infoMap
      . externInfoMap
      %~ (`Map.union` mp)

emptyInfoMap :: InfoMap
emptyInfoMap =
  InfoMap
    { _relationInfoMap = Map.empty
    , _relationParamKindInfoMap = Map.empty
    , _partialOrdInfoMap = Map.empty
    , _ruleInfoMap = Map.empty
    , _queryInfoMap = Map.empty
    , _externInfoMap = Map.empty
    }

data Kind = Discrete | PartiallyOrdered
  deriving (Show, Eq)

data RelationArgMatchInfo = Unmatched | Matched NodeId NodeId
  deriving
    (Show, Eq)

data RuleInfo = RuleInfo
  { _ruleDeclaration :: Rule
  , _ruleBoundVars :: RepresentativeMap LocalVarInfo
  }
  deriving (Show, Eq)

ruleDeclaration :: Lens' RuleInfo Rule
ruleDeclaration = lens _ruleDeclaration (\ri r -> ri {_ruleDeclaration = r})

ruleBoundVars :: Lens' RuleInfo (RepresentativeMap LocalVarInfo)
ruleBoundVars = lens _ruleBoundVars (\ri r -> ri {_ruleBoundVars = r})

data RelationInfo = RelationInfo
  { _relationDeclaration :: Relation
  , _relationArgMatchInfo :: [RelationArgMatchInfo]
  }
  deriving (Show, Eq)

relationDeclaration :: Lens' RelationInfo Relation
relationDeclaration = lens _relationDeclaration (\ri r -> ri {_relationDeclaration = r})

relationArgMatchInfo :: Lens' RelationInfo [RelationArgMatchInfo]
relationArgMatchInfo = lens _relationArgMatchInfo (\ri r -> ri {_relationArgMatchInfo = r})

data TypeLattice
  = Dynamic
  | ActualType
      Representative
      -- ^ The RelationArg
      Assumption
      -- ^ First discovered usage that gives us information about why the type
      -- is as such
  | Top
  deriving (Show, Eq)

data LocalVarInfo = LocalVarInfo
  { _localVarType :: TypeLattice
  , _localVarScope :: Representative
  -- ^ Where it is declared (rule/priority representative).
  }
  deriving (Show, Eq)

localVarType :: Lens' LocalVarInfo TypeLattice
localVarType = lens _localVarType (\lv t -> lv {_localVarType = t})

localVarScope :: Lens' LocalVarInfo Representative
localVarScope = lens _localVarScope (\lv t -> lv {_localVarScope = t})

emptySymbolEnv :: SymbolEnv
emptySymbolEnv =
  SymbolEnv
    { _infoMap = emptyInfoMap
    , _nodeMap = IntMap.empty
    }

type Symboled σ = σ :>: SymbolEnv
