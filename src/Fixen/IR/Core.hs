{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict #-}

-- |
--     Module      : Fixen.IR.Core
--     Description : Building blocks for Fixen intermediate representations
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     This module defines the core data structures that form the building
--     blocks of Fixen's intermediate representation (IR). These types are
--     polymorphic in their name and parameter types, allowing the parser
--     to instantiate them with concrete types in 'Fixen.IR.AST'.
--
--     The types are organized into three groups:
--
--     * /Identifiers and expressions/ — 'SimpleIdentifier', 'ModuleName',
--       'FullyQualifiedName', 'Identifier', 'Expr', 'Type'
--
--     * /Program constructs/ — 'Relation', 'Rule', 'Condition', 'HsBlock',
--       'HsImport', 'Include', 'Priority', 'PriorityConclusion', 'RuleInstance',
--       'Query', 'QueryMode', 'ModuleDeclaration', 'Extern',
--       'PartialOrdDeclaration', 'Phases', 'Ruleset', 'Program'
--
--     Every type carries a 'NodeId' for source position tracking and
--     implements 'HasNodeId'. Most named constructs also implement the
--     'Named' class for extracting their name.
--
--     The polymorphic type parameters use the following convention:
--
--     * @ν@ — a name type (e.g. 'SimpleIdentifier', 'ModuleName')
--     * @β@ — bound variables
--     * @π@ — parameters (e.g. type arguments)
--     * @χ@ — conditions
--     * @δ@ — conclusions
--     * @ε@ — expressions
--     * @ω@ — priority conclusions
--     * @ρ@ — rule references
--     * @μ@ — maps (e.g. variable substitutions)
--     * @τ@ — types
--     * @ℓ@ — less-than-or-equal functions
--     * @μ@ — maximal lower bounds functions
--     * @φ@ — phase declarations
--     * @σ@ — source types
--     * @ι@ — import types
--     * @β@ — block contents
--     * @ν@ — include paths
--     * @ε@ — extern symbols
--     * @ρ@ — relations
--     * @π@ — partial order declarations
--     * @σ@ — rules
--     * @ω@ — priorities
--     * @χ@ — queries
--     * @φ@ — phases
module Fixen.IR.Core (
  -- * Identifiers
  SimpleIdentifier (..),
  ModuleName (..),
  FullyQualifiedName (..),
  Identifier (.., MkIdentifierSimple, MkIdentifierFQN),
  IdentifierLike (..),

  -- * Expressions and types
  Expr (..),
  Type (..),

  -- * Program constructs
  Relation (..),
  Rule (..),
  Condition (..),
  HsBlock (..),
  HsImport (..),
  Include (..),
  Priority (..),
  PriorityConclusion (..),
  RuleInstance (..),
  Query (..),
  QueryMode (..),
  ModuleDeclaration (..),
  Extern (..),
  PartialOrdDeclaration (..),
  Phases (..),
  Named (..),
  Ruleset (..),
  Program (..),
) where

import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text, append, cons, intercalate, pack)
import Fixen.Data.NodeId (EqModuloNodeId (..), HasNodeId (..), NodeId)

import GHC.Natural (Natural)

-------------------------------------------------------------------------------
--
-- Identifier types
--
-------------------------------------------------------------------------------

-- | Class for types that represent identifiers in the program.
--
--   An 'IdentifierLike' value can be queried for:
--
--   * Its /simple/ name — the last component of the identifier
--     (e.g. @a@ from @My.Module.a@)
--   * Its /full/ name — the complete qualified name
--     (e.g. @My.Module.a@)
--
--   This class is implemented by 'SimpleIdentifier', 'ModuleName',
--   'FullyQualifiedName', and 'Identifier'.
class IdentifierLike σ where
  -- | Extract the simple (unqualified) name from an identifier.
  --
  --   For a fully-qualified name like @My.Module.a@, this returns @a@.
  --   For a simple identifier like @a@, this returns @a@ unchanged.
  --   For a 'ModuleName' like @Data.List@, this returns the entire
  --   module name as a single 'Text' value (@Data.List@).
  simpleIdentifier :: σ -> Text

  -- | Extract the full (potentially qualified) name from an identifier.
  --
  --   For a fully-qualified name like @My.Module.a@, this returns
  --   @My.Module.a@. For a simple identifier like @a@, this returns
  --   @a@ unchanged.
  fullIdentifier :: σ -> Text

-- | A simple (unqualified) identifier in the program.
--
--   This wraps a 'Text' value with a 'NodeId' for source position tracking.
--   Simple identifiers are used for variable names, relation names, rule names,
--   and the final component of fully-qualified names.
--
--   Examples: @x@, @myFunction@, @Data@, @Just@
data SimpleIdentifier = SimpleIdentifier
  { simpleIdentifierNodeId :: NodeId
  -- ^ The 'NodeId' attached to this 'SimpleIdentifier', used for
  --   source position tracking.
  , simpleIdentifierName :: Text
  -- ^ The identifier text (e.g. @x@, @myFunction@, @Data@).
  }
  deriving (Show, Eq)

-- | 'SimpleIdentifier's are equal modulo 'NodeId's whenever their
-- texts are equal
instance EqModuloNodeId SimpleIdentifier where
  a === b = simpleIdentifierName a == simpleIdentifierName b

-- | Order simple identifiers lexicographically by their name.
instance Ord SimpleIdentifier where
  SimpleIdentifier {simpleIdentifierName = n} <= SimpleIdentifier {simpleIdentifierName = n'} = n <= n'

-- | A 'SimpleIdentifier' is its own node ID.
instance HasNodeId SimpleIdentifier where
  getNodeId = simpleIdentifierNodeId

-- | A 'SimpleIdentifier' is trivially 'IdentifierLike': both its simple
-- and full names are just its text value.
instance IdentifierLike SimpleIdentifier where
  simpleIdentifier = simpleIdentifierName
  fullIdentifier = simpleIdentifierName

-- | A Haskell-style module name, consisting of one or more capitalized
-- identifiers separated by dots.
--
--   This wraps a 'NonEmpty' list of 'SimpleIdentifier' values with a
--   'NodeId' for source position tracking.
--
--   Examples: @Data@, @Data.List@, @MyCompany.MyProject.MyModule@
data ModuleName = ModuleName
  { moduleNodeId :: NodeId
  -- ^ The 'NodeId' attached to this 'ModuleName', used for source
  --   position tracking.
  , moduleNameName :: NonEmpty SimpleIdentifier
  -- ^ The individual components of the module name (e.g. @Data@ and
  --   @List@ for @Data.List@).
  }
  deriving (Show, Eq)

-- | A 'ModuleName' is its own node ID.
instance HasNodeId ModuleName where
  getNodeId = moduleNodeId

-- | A 'ModuleName' is 'IdentifierLike': its simple and full names are
-- both its components joined by dots.
instance IdentifierLike ModuleName where
  simpleIdentifier m = Data.Text.intercalate (pack ".") (toList $ simpleIdentifier <$> moduleNameName m)
  fullIdentifier = simpleIdentifier

-- | 'ModuleName's are equal modulo 'NodeIds' whenever their names are.
instance EqModuloNodeId ModuleName where
  a === b = moduleNameName a === moduleNameName b

-- | A fully-qualified name in the program, consisting of a 'ModuleName'
-- prefix and a final 'SimpleIdentifier'.
--
--   This wraps the three components (node ID, module name, final name)
--   for source position tracking.
--
--   Examples: @Data.List.map@, @MyModule.MyType@
data FullyQualifiedName = FullyQualifiedName
  { fqnNodeId :: NodeId
  -- ^ The 'NodeId' attached to this 'FullyQualifiedName', used for
  --   source position tracking.
  , fqnModuleName :: ModuleName
  -- ^ The module prefix (e.g. @Data.List@ in @Data.List.map@).
  , fqnName :: SimpleIdentifier
  -- ^ The final name component (e.g. @map@ in @Data.List.map@).
  }
  deriving (Show, Eq)

-- | A 'FullyQualifiedName' is its own node ID.
instance HasNodeId FullyQualifiedName where
  getNodeId = fqnNodeId

-- | 'FullyQualifiedName's are equal modulo 'NodeId's whenever their
-- module names and names are.
instance EqModuloNodeId FullyQualifiedName where
  a === b = fqnModuleName a === fqnModuleName b && fqnName a === fqnName b

-- | A 'FullyQualifiedName' is 'IdentifierLike': its simple name is
-- the final component, and its full name is the module prefix joined
-- with the final component by a dot.
instance IdentifierLike FullyQualifiedName where
  simpleIdentifier = simpleIdentifier . fqnName
  fullIdentifier n = append (fullIdentifier (fqnModuleName n)) (cons '.' (simpleIdentifier (fqnName n)))

-- | An identifier in the program, which is either a simple identifier
-- or a fully-qualified name.
--
--   This is a sum type used in contexts where either form is valid,
--   such as expression variables.
data Identifier
  = IdentifierSimpleIdentifier
      { identifierSimpleIdentifier :: SimpleIdentifier
      -- ^ The simple identifier value.
      }
  | IdentifierFullyQualifiedName
      { identifierFQN :: FullyQualifiedName
      -- ^ The fully-qualified name value.
      }
  deriving (Show, Eq)

-- | Pattern synonym for constructing a simple identifier.
--
--   @
--   MkIdentifierSimple :: NodeId -> Text -> Identifier
--   @
--
--   Example: @MkIdentifierSimple 42 "x"@ produces 'IdentifierSimpleIdentifier'
--   wrapping a 'SimpleIdentifier' with node ID @42@ and name @"x"@.
pattern MkIdentifierSimple
  :: NodeId
  -- ^ The 'NodeId' for the identifier
  -> Text
  -- ^ The identifier text
  -> Identifier
pattern MkIdentifierSimple uniq s = IdentifierSimpleIdentifier (SimpleIdentifier uniq s)

-- | Pattern synonym for constructing a fully-qualified identifier.
--
--   @
--   MkIdentifierFQN :: NodeId -> ModuleName -> SimpleIdentifier -> Identifier
--   @
--
--   Example: @MkIdentifierFQN 42 (ModuleName [Data, List]) map@ produces
--   'IdentifierFullyQualifiedName' wrapping a 'FullyQualifiedName' for @Data.List.map@.
pattern MkIdentifierFQN
  :: NodeId
  -- ^ The 'NodeId' for the identifier
  -> ModuleName
  -- ^ The module name prefix
  -> SimpleIdentifier
  -- ^ The final name component
  -> Identifier
pattern MkIdentifierFQN uniq mod i = IdentifierFullyQualifiedName (FullyQualifiedName uniq mod i)

-- | Ensure that all 'Identifier' values can be constructed via the two
-- pattern synonyms. This enables exhaustive pattern matching.
{-# COMPLETE MkIdentifierSimple, MkIdentifierFQN #-}

-- | An 'Identifier' is its own node ID, extracting from whichever
-- constructor is active.
instance HasNodeId Identifier where
  getNodeId (MkIdentifierSimple u _) = u
  getNodeId (MkIdentifierFQN u _ _) = u

-- | An 'Identifier' is 'IdentifierLike': delegates to the inner value's
-- implementation.
instance IdentifierLike Identifier where
  simpleIdentifier (IdentifierSimpleIdentifier i) = simpleIdentifier i
  simpleIdentifier (IdentifierFullyQualifiedName fqn) = simpleIdentifier fqn
  fullIdentifier (IdentifierSimpleIdentifier i) = fullIdentifier i
  fullIdentifier (IdentifierFullyQualifiedName i) = fullIdentifier i

-- | 'Identifier's are equal modulo 'NodeIds' whenever their components are.
instance EqModuloNodeId Identifier where
  MkIdentifierSimple _ s === MkIdentifierSimple _ t = s == t
  MkIdentifierFQN _ a b === MkIdentifierFQN _ c d = a === c && b === d
  _ === _ = False

-- | An expression in the Fixen language.
--
--   Expressions appear in rule conditions, priority premises, and
--   conclusion arguments. The type supports variables, application
--   (function application), integer and string literals, tuples,
--   lists, and the unit value.
data Expr where
  -- | A variable expression, referencing an identifier.
  ExprVar
    :: NodeId
    -- ^ The 'NodeId' for source position tracking.
    -> Identifier
    -- ^ The identifier being referenced.
    -> Expr
  -- | A function application expression (@f x@).
  ExprApp
    :: NodeId
    -- ^ The 'NodeId' for source position tracking.
    -> Expr
    -- ^ The function being applied.
    -> Expr
    -- ^ The argument.
    -> Expr
  -- | An integer literal expression.
  ExprIntLit
    :: NodeId
    -- ^ The 'NodeId' for source position tracking.
    -> Integer
    -- ^ The integer value.
    -> Expr
  -- | A string literal expression.
  ExprStrLit
    :: NodeId
    -- ^ The 'NodeId' for source position tracking.
    -> Text
    -- ^ The string value.
    -> Expr
  -- | A tuple expression. The tuple must have at least two elements.
  ExprTuple
    :: NodeId
    -- ^ The 'NodeId' for source position tracking.
    -> Expr
    -- ^ The first element of the tuple.
    -> NonEmpty Expr
    -- ^ The remaining elements of the tuple (non-empty, ensuring at least 2 total).
    -> Expr
  -- | A list expression.
  ExprList
    :: NodeId
    -- ^ The 'NodeId' for source position tracking.
    -> [Expr]
    -- ^ The elements of the list (may be empty).
    -> Expr
  -- | The unit value @()@.
  ExprUnit
    :: NodeId
    -- ^ The 'NodeId' for source position tracking.
    -> Expr
  deriving (Show, Eq)

-- | Every 'Expr' constructor carries a 'NodeId' for source position tracking.
instance HasNodeId Expr where
  getNodeId (ExprVar u _) = u
  getNodeId (ExprApp u _ _) = u
  getNodeId (ExprIntLit u _) = u
  getNodeId (ExprStrLit u _) = u
  getNodeId (ExprTuple u _ _) = u
  getNodeId (ExprList u _) = u
  getNodeId (ExprUnit u) = u

-- | 'Expr's are equal modulo 'NodeId's whenever their components are.
instance EqModuloNodeId Expr where
  ExprVar _ v === ExprVar _ v' = v === v'
  ExprApp _ f x === ExprApp _ f' x' = f === f' && x === x'
  ExprIntLit _ i === ExprIntLit _ i' = i == i'
  ExprStrLit _ s === ExprStrLit _ s' = s == s'
  ExprTuple _ h t === ExprTuple _ h' t' = h === h' && t === t'
  ExprList _ l === ExprList _ l' = l === l'
  ExprUnit _ === ExprUnit _ = True
  _ === _ = False

-- | A type in the Fixen language.
--
--   Types appear as parameters to relation declarations. The type system
--   supports named types, type application, built-in list and tuple types,
--   the unit type, and literal types (natural numbers and symbols).
data Type where
  -- | A named type, referencing an identifier (e.g. @Int@, @MyType@).
  TypeName
    :: NodeId
    -- ^ The 'NodeId' for source position tracking.
    -> Identifier
    -- ^ The type name.
    -> Type
  -- | Type application (@T1 T2@), representing a type constructor applied
  -- to an argument type.
  TypeApp
    :: NodeId
    -- ^ The 'NodeId' for source position tracking.
    -> Type
    -- ^ The type constructor (LHS).
    -> Type
    -- ^ The argument type (RHS).
    -> Type
  -- | A list type (@[a]@). This is a built-in type, distinct from
  -- generic type application, for convenience in pretty-printing and
  -- code generation.
  TypeList
    :: NodeId
    -- ^ The 'NodeId' for source position tracking.
    -> Type
    -- ^ The element type.
    -> Type
  -- | A tuple type (@(a, b, ...)@). This is a built-in type, distinct
  -- from generic type application, for convenience in pretty-printing
  -- and code generation. The tuple must have at least two elements.
  TypeTuple
    :: NodeId
    -- ^ The 'NodeId' for source position tracking.
    -> Type
    -- ^ The type of the first element.
    -> NonEmpty Type
    -- ^ The types of the remaining elements (non-empty, ensuring at least 2 total).
    -> Type
  -- | The unit type @()@.
  TypeUnit
    :: NodeId
    -- ^ The 'NodeId' for source position tracking.
    -> Type
  -- | A natural number literal type (e.g. @0@, @1@, @42@).
  TypeNatLit
    :: NodeId
    -- ^ The 'NodeId' for source position tracking.
    -> Natural
    -- ^ The natural number value.
    -> Type
  -- | A string/symbol literal type (e.g. @@"foo"@@).
  TypeSymbolLit
    :: NodeId
    -- ^ The 'NodeId' for source position tracking.
    -> Text
    -- ^ The symbol/string value.
    -> Type
  deriving (Show, Eq)

-- | Every 'Type' constructor carries a 'NodeId' for source position tracking.
instance HasNodeId Type where
  getNodeId (TypeName u _) = u
  getNodeId (TypeApp u _ _) = u
  getNodeId (TypeNatLit u _) = u
  getNodeId (TypeSymbolLit u _) = u
  getNodeId (TypeUnit u) = u
  getNodeId (TypeTuple u _ _) = u
  getNodeId (TypeList u _) = u

-- | 'Type's are equal modulo 'NodeId's whenever their components are.
instance EqModuloNodeId Type where
  TypeName _ v === TypeName _ v' = v === v'
  TypeApp _ f x === TypeApp _ f' x' = f === f' && x === x'
  TypeNatLit _ i === TypeNatLit _ i' = i == i'
  TypeSymbolLit _ s === TypeSymbolLit _ s' = s == s'
  TypeTuple _ h t === TypeTuple _ h' t' = h === h' && t === t'
  TypeList _ l === TypeList _ l' = l === l'
  TypeUnit _ === TypeUnit _ = True
  _ === _ = False

--------------------------------------------------------------------------------
--
-- Program constructs
--
--------------------------------------------------------------------------------

-- | Class for program constructs that have a name.
--
--   The functional dependency @α -> ν@ ensures that each construct type
--   @α@ has exactly one name type @ν@. For example, a 'Relation' has
--   a 'SimpleIdentifier' name, while a 'Rule' has an optional name.
--
--   This class enables generic name extraction across different construct
--   types, useful for pretty-printing, error reporting, and indexing.
class Named α ν | α -> ν where
  -- | Extract the name of a program construct.
  nameOf :: α -> ν

-- | A relation declaration in the program.
--
--   Relations represent facts or predicates that can be assumed or concluded
--   in rules. They have a name (typically capitalized, as they are
--   constructor-like) and a list of argument types.
--
--   Example:
--
--   @
--   rel Dist
--       : Integer,
--       Integer
--   @
data Relation β π = Relation
  { relationNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , relationName :: β
  -- ^ The relation name (e.g. @Dist@, @MyFact@).
  , relationParams :: π
  -- ^ The types of the relation's arguments (e.g. @[Integer, Integer]@).
  }
  deriving (Show, Eq)

-- | A 'Relation' is its own node ID.
instance HasNodeId (Relation β π) where
  getNodeId = relationNodeId

-- | A 'Relation' is named by its relation name.
instance Named (Relation β π) β where
  nameOf = relationName

-- | 'Relation's are equal modulo 'NodeId's whenever their components are.
instance (EqModuloNodeId β, EqModuloNodeId π) => EqModuloNodeId (Relation β π) where
  r === r' = relationName r === relationName r' && relationParams r === relationParams r'

-- | A rule declaration in the program.
--
--   Rules define inference: given a set of assumptions and conditions,
--   a conclusion follows. Rules have:
--
--   * An optional name (for reference in phases and priorities)
--   * Optional bound variables (for use in the rule body)
--   * A list of assumptions (relations applied to variables)
--   * A list of conditions (expressions guarded by 'if')
--   * A conclusion (a relation applied to expressions)
--
--   Example:
--
--   @
--   rule myRule x y:
--       MyFact x,
--       if x < y,
--       |- Dist x y
--   @
data Rule ν β π χ δ = Rule
  { ruleNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , ruleName :: ν
  -- ^ The optional rule name (e.g. @myRule@, or @Nothing@ if unnamed).
  , ruleBoundVars :: β
  -- ^ The bound variables of the rule (e.g. @[x, y]@).
  , ruleAssumptions :: π
  -- ^ The assumptions (relations applied to variables).
  , ruleConditions :: χ
  -- ^ The conditions (expressions guarded by 'if').
  , ruleConclusion :: δ
  -- ^ The conclusion (a relation applied to expressions).
  }
  deriving (Show, Eq)

-- | 'Rule's are equal modulo 'NodeId's whenever their components are.
instance
  ( EqModuloNodeId ν
  , EqModuloNodeId β
  , EqModuloNodeId π
  , EqModuloNodeId χ
  , EqModuloNodeId δ
  )
  => EqModuloNodeId (Rule ν β π χ δ)
  where
  r === r' =
    ruleName r === ruleName r'
      && ruleBoundVars r === ruleBoundVars r'
      && ruleAssumptions r === ruleAssumptions r'
      && ruleConditions r === ruleConditions r'
      && ruleConclusion r === ruleConclusion r'

-- | A 'Rule' is its own node ID.
instance HasNodeId (Rule ν β π χ δ) where
  getNodeId = ruleNodeId

-- | A 'Rule' is named by its rule name.
instance Named (Rule ν β π χ δ) ν where
  nameOf = ruleName

-- | A condition within a rule body.
--
--   Conditions are expressions guarded by the 'if' keyword. They evaluate
--   to a boolean truth value that must hold for the rule to fire.
--
--   Example: @if a <= b@
data Condition ε = Condition
  { conditionNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , conditionExpr :: ε
  -- ^ The condition expression.
  }
  deriving (Show, Eq)

-- | A 'Condition' is its own node ID.
instance HasNodeId (Condition ε) where
  getNodeId = conditionNodeId

-- | A Haskell source code block embedded in a Fixen program.
--
--   Haskell blocks are delimited by triple backticks with the @hs@
--   language annotation (```hs ... ```). They allow defining Haskell
--   code that is referenced by 'extern' declarations.
--
--   Example:
--
--   @
--   ```hs
--   myHaskellFunction :: Int -> Int
--   myHaskellFunction x = x + 1
--   ```
--   @
data HsBlock β = HsBlock
  { hsBlockNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , hsBlockContents :: β
  -- ^ The Haskell source code contents.
  }
  deriving (Show, Eq)

-- | A 'HsBlock' is its own node ID.
instance HasNodeId (HsBlock β) where
  getNodeId = hsBlockNodeId

-- | A Haskell module import statement.
--
--   Import statements allow a Fixen program to reference Haskell symbols
--   defined in external modules. The imported module is specified as a
--   'ModuleName'.
--
--   Example:
--
--   @
--   import Data.List
--   @
data HsImport σ = HsImport
  { hsImportNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , hsImportImport :: σ
  -- ^ The imported module name (e.g. @Data.List@).
  }
  deriving (Show, Eq)

-- | A 'HsImport' is its own node ID.
instance HasNodeId (HsImport σ) where
  getNodeId = hsImportNodeId

-- | An @include@ statement for importing another Fixen program.
--
--   Include statements allow one Fixen program to reuse declarations
--   from another Fixen file. The included file path is a string literal.
--
--   Example:
--
--   @
--   include "Path/To/Fixen.fix"
--   @
data Include π = Include
  { includeNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , includePath :: π
  -- ^ The path to the included Fixen file.
  }
  deriving (Show, Eq)

-- | An 'Include' is its own node ID.
instance HasNodeId (Include π) where
  getNodeId = includeNodeId

-- | A priority declaration.
--
--   Priority declarations specify an ordering between rule instances.
--   When multiple rules could fire, priorities determine which one
--   takes precedence. A priority consists of:
--
--   * A premise expression that must hold
--   * A conclusion comparing two rule instances with an ordering symbol
--
--   Example:
--
--   @
--   priority:
--       a <= b |- addDist { a = a, b = b' } <= addDist { a = a', b = b' }
--   @
data Priority ε ω = Priority
  { priorityNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , priorityPremise :: ε
  -- ^ The premise expression that must hold for the priority to apply.
  , priorityConclusion :: ω
  -- ^ The conclusion comparing two rule instances with an ordering.
  }
  deriving (Show, Eq)

-- | A 'Priority' is its own node ID.
instance HasNodeId (Priority ε ω) where
  getNodeId = priorityNodeId

-- | The conclusion of a priority declaration.
--
--   This compares two rule instances with an ordering symbol (@<=@ or
--   @⊏@), specifying that the left-hand side instance has lower or
--   equal priority than the right-hand side.
data PriorityConclusion ℓ ρ = PriorityConclusion
  { priorityConclusionNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , priorityConclusionLHS :: ℓ
  -- ^ The left-hand side rule instance.
  , priorityConclusionRHS :: ρ
  -- ^ The right-hand side rule instance.
  }
  deriving (Show, Eq)

-- | A 'PriorityConclusion' is its own node ID.
instance HasNodeId (PriorityConclusion ℓ ρ) where
  getNodeId = priorityConclusionNodeId

-- | The instantiation of a rule, used in priority declarations.
--
--   A rule instance consists of a rule name and an optional map of
--   variable substitutions. The map describes how the rule's bound
--   variables are instantiated with specific values.
--
--   Example: @addDist { a = a, b = b' }@ instantiates the @addDist@
--   rule with @a@ mapping to @a@ and @b@ mapping to @b'@.
data RuleInstance ρ μ = RuleInstance
  { ruleInstanceNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , ruleInstanceRule :: ρ
  -- ^ The rule being instantiated.
  , ruleInstanceMap :: μ
  -- ^ The variable substitution map.
  }
  deriving (Show, Eq)

-- | A 'RuleInstance' is its own node ID.
instance HasNodeId (RuleInstance ρ μ) where
  getNodeId = ruleInstanceNodeId

-- | A 'RuleInstance' is named by its rule reference.
instance Named (RuleInstance ρ μ) ρ where
  nameOf = ruleInstanceRule

-- | A query declaration.
--
--   Query declarations specify how to query a relation, indicating which
--   arguments are inputs and which are outputs. Each query has:
--
--   * A relation name (capitalized)
--   * A query name (lowercase, for reference)
--   * A list of modes (@+@ for input, @-@ for output)
--
--   Example:
--
--   @
--   query DistTo as distTo - +
--   @
data Query ρ ν = Query
  { queryNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , queryRel :: ρ
  -- ^ The relation being queried.
  , queryName :: ν
  -- ^ The query name (lowercase identifier for reference).
  }
  deriving (Show, Eq)

-- | A 'Query' is named by its query name.
instance Named (Query ρ ν) ν where
  nameOf = queryName

-- | A 'Query' is its own node ID.
instance HasNodeId (Query ρ ν) where
  getNodeId = queryNodeId

-- | The mode of a query argument: input (@+@) or output (@-@).
--
--   Query modes specify whether each argument of a relation is an input
--   (ground value to be matched) or an output (value to be computed).
--
--   * 'Input' — the argument is an input variable (ground)
--   * 'Output' — the argument is an output variable (to be computed)
data QueryMode = Input NodeId | Output NodeId
  deriving (Show, Eq)

-- | A 'QueryMode' is its own node ID.
instance HasNodeId QueryMode where
  getNodeId (Input u) = u
  getNodeId (Output u) = u

-- | A module declaration, specifying the Haskell module name that will
-- be generated from the Fixen program.
--
--   The module declaration is the first line of every Fixen program:
--
--   @
--   module My.Haskell.Module where
--   @
--
--   The module name determines the name of the generated Haskell module.
data ModuleDeclaration ν = ModuleDeclaration
  { moduleDeclarationNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , moduleDeclarationName :: ν
  -- ^ The Haskell module name being generated.
  }
  deriving (Show, Eq)

-- | A 'ModuleDeclaration' is its own node ID.
instance HasNodeId (ModuleDeclaration ν) where
  getNodeId = moduleDeclarationNodeId

-- | A 'ModuleDeclaration' is named by its module name.
instance Named (ModuleDeclaration ν) ν where
  nameOf = moduleDeclarationName

-- | An extern declaration, listing symbols defined in Haskell code.
--
--   Extern declarations inform Fixen which symbols are defined in
--   Haskell source blocks or imported modules, so that the symbol
--   solver can resolve references to them.
--
--   Example:
--
--   @
--   extern
--       myHaskellFunction,
--       anotherSymbol
--   @
data Extern δ = Extern
  { externNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , externSymbols :: δ
  -- ^ The list of externally defined symbols.
  }
  deriving (Show, Eq)

-- | An 'Extern' is its own node ID.
instance HasNodeId (Extern δ) where
  getNodeId = externNodeId

-- | A partial order declaration.
--
--   Partial order declarations define the instance of 'PartialOrd' for
--   a specific type, informing Fixen how to:
--
--   * Compare elements of the type (via the 'leq' function)
--   * Compute maximal lower bounds of two elements (via the 'mlbs' function)
--
--   This is used by Fixen to generate optimized database representations
--   and to support priority-based rule evaluation.
--
--   Example:
--
--   @
--   partial ord Dist
--       where
--       type = Dist
--       leq = (<=)
--       mlbs = meet
--   @
data PartialOrdDeclaration ν τ ℓ μ = PartialOrdDeclaration
  { partialOrdDeclarationNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , partialOrdDeclarationName :: ν
  -- ^ The type being defined as a partial order.
  , partialOrdDeclarationType :: τ
  -- ^ The base type of the partial order.
  , partialOrdDeclarationLeq :: ℓ
  -- ^ The less-than-or-equal comparison function.
  , partialOrdDeclarationMlbs :: μ
  -- ^ The maximal lower bounds function.
  }
  deriving (Show, Eq)

-- | A 'PartialOrdDeclaration' is its own node ID.
instance HasNodeId (PartialOrdDeclaration ν τ ℓ μ) where
  getNodeId = partialOrdDeclarationNodeId

-- | A 'PartialOrdDeclaration' is named by its type name.
instance Named (PartialOrdDeclaration ν τ ℓ μ) ν where
  nameOf = partialOrdDeclarationName

-- | A phase declaration for multi-phase FPOP (Fixed Point Over lattices)
-- evaluation.
--
--   Phase declarations organize rules into execution phases, enabling
--   optimizations such as reduced product and widening. Each phase
--   contains either an explicit set of rule names or the wildcard
--   @*@ (all remaining rules).
--
--   Example:
--
--   @
--   phases:
--       [ { rule1, rule2 }, { rule3 }, * ]
--   @
data Phases φ = Phases
  { phasesNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , phasesPhases :: φ
  -- ^ The phase declarations (list of rulesets).
  }
  deriving (Show, Eq)

-- | A 'Phases' is its own node ID.
instance HasNodeId (Phases φ) where
  getNodeId = phasesNodeId

-- | A set of rule names, representing the contents of a phase declaration.
--
--   A ruleset is either an explicit list of rule names or the wildcard
--   @*@ (all remaining rules). Rulesets are used to group rules into
--   execution phases.
data Ruleset ρ = Ruleset
  { ruleSetNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , ruleSetRules :: ρ
  -- ^ The rule names in this ruleset.
  }
  deriving (Show, Eq)

-- | A 'Ruleset' is its own node ID.
instance HasNodeId (Ruleset ρ) where
  getNodeId = ruleSetNodeId

-- | A complete Fixen program.
--
--   The 'Program' type is a polymorphic record containing all parts of
--   a Fixen program, from module declaration through queries and phases.
--   Each field is polymorphic to allow the parser to use concrete types
--   (defined in 'Fixen.IR.AST') while keeping the core types generic.
--
--   The fields are:
--
--   * 'moduleName' — the Haskell module declaration
--   * 'hsImports' — Haskell module imports
--   * 'hsBlocks' — embedded Haskell source code blocks
--   * 'includes' — included Fixen files
--   * 'extern' — externally defined symbols
--   * 'relations' — relation declarations
--   * 'partialOrdDeclarations' — partial order declarations
--   * 'rules' — rule declarations
--   * 'priorities' — priority declarations
--   * 'queries' — query declarations
--   * 'phases' — phase declarations (optional)
data Program μ ι β ν ε ρ π σ ω χ φ = Program
  { moduleName :: μ
  -- ^ The module declaration (e.g. @module My.Haskell.Module where@).
  , hsImports :: ι
  -- ^ Haskell module imports.
  , hsBlocks :: β
  -- ^ Embedded Haskell source code blocks.
  , includes :: ν
  -- ^ Included Fixen files.
  , extern :: ε
  -- ^ Externally defined symbols.
  , relations :: ρ
  -- ^ Relation declarations.
  , partialOrdDeclarations :: π
  -- ^ Partial order declarations.
  , rules :: σ
  -- ^ Rule declarations.
  , priorities :: ω
  -- ^ Priority declarations.
  , queries :: χ
  -- ^ Query declarations.
  , phases :: φ
  -- ^ Phase declarations (optional; 'Nothing' if no phases declared).
  }
  deriving (Show, Eq)
