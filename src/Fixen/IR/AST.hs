{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
--     Module      : Fixen.IR.AST
--     Description : Abstract syntax trees
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     This module defines the /concrete/ data structures produced by the
--     parser. It instantiates the polymorphic types from 'Fixen.IR.Core'
--     with the specific types the parser generates, yielding a fully
--     typed abstract syntax tree (AST) ready for subsequent compiler
--     passes (symbol solving, type checking, code generation).
--
--     /Type aliases./ Every type in this module is a type alias (or
--     newtype) that fixes one or more type parameters of a 'Core' type.
--     For example, 'Relation' is 'Core.Relation' instantiated with
--     'Core.SimpleIdentifier' for the name and @[Core.Type]@ for the
--     argument types. This separation allows the core types to remain
--     generic while the parser produces concrete, fully-resolved AST
--     nodes.
--
--     /Pretty-printing./ This module also provides a suite of
--     pretty-printing functions using the @prettyprinter@ library.
--     These functions format AST nodes for human-readable output,
--     with syntax highlighting via 'Doc' annotations.
module Fixen.IR.AST where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text, append, cons, intercalate, pack)

-- import Data.Text qualified as Text
import GHC.Natural
import Prettyprinter
import Prettyprinter.Render.Terminal

-- | A unique identifier assigned to every construct in the Fixen IR.
--
--   Internally this is represented as an 'Int'. Values are allocated
--   sequentially starting from 0 by the parser, so they are always
--   non-negative and strictly increasing within a single parse run.
--
--   A 'NodeId' is /stable/ — it does not change when the surrounding AST
--   is transformed or pretty-printed. This makes it suitable as a key
--   for maps that store auxiliary information (e.g. symbol tables,
--   position maps, diagnostic data) alongside the IR.
type NodeId = Int

-- | Class of IR terms that carry a 'NodeId'.
--
--   Almost every data type in the Fixen IR ('Fixen.IR.Core') is an
--   instance of this class. The class provides a single method,
--   'getNodeId', which extracts the identifier from a term.
--
--   This enables generic code to work uniformly with any IR node
--   without needing to know its concrete type. For example, a
--   function that collects all node identifiers in a subtree can be
--   written once using 'HasNodeId' rather than pattern-matching on
--   every IR constructor.
class HasNodeId α where
  -- | Extract the 'NodeId' from a term.
  --
  --   For IR constructors that store the node identifier as their first
  --   field (the common convention in this codebase), the instance
  --   implementation is typically a one-liner:
  --
  --   @
  --   instance HasNodeId MyType where
  --     getNodeId (MyType i _) = i
  --   @
  --
  --   Or even simpler, when the identifier is the sole field:
  --
  --   @
  --   instance HasNodeId MyType where
  --     getNodeId = myTypeNodeId   -- point-free
  --   @
  getNodeId :: α -> NodeId

instance HasNodeId NodeId where
  getNodeId = id

-- | We want to ensure that we can compare two items for equality modulo having
-- different 'NodeId's. This is used to equate things together for easy comparison.
class EqModuloNodeId α where
  -- | Equality modulo 'NodeId's.
  (===) :: α -> α -> Bool
  a === b = not (a /== b)

  -- | Disequality modulo 'NodeId's.
  (/==) :: α -> α -> Bool
  a /== b = not (a === b)

  {-# MINIMAL (===) | (/==) #-}

infix 4 ===
infix 4 /==

instance EqModuloNodeId α => EqModuloNodeId (Maybe α) where
  Nothing === Nothing = True
  Just x === Just y = x === y
  _ === _ = False

instance EqModuloNodeId α => EqModuloNodeId [α] where
  [] === [] = True
  (x : xs) === (y : ys) = x === y && xs === ys
  _ === _ = False

instance EqModuloNodeId α => EqModuloNodeId (NonEmpty α) where
  (x :| xs) === (y :| ys) = x === y && xs === ys

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
  simpleIdentifier m = intercalate (pack ".") (NonEmpty.toList $ simpleIdentifier <$> moduleNameName m)
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
data RelationLike π = Relation
  { relationNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , relationName :: SimpleIdentifier
  -- ^ The relation name (e.g. @Dist@, @MyFact@).
  , relationParams :: [π]
  -- ^ The types of the relation's arguments (e.g. @[Integer, Integer]@).
  }
  deriving (Show, Eq)

-- | A 'Relation' is its own node ID.
instance HasNodeId (RelationLike π) where
  getNodeId = relationNodeId

-- | A 'Relation' is named by its relation name.
instance Named (RelationLike π) SimpleIdentifier where
  nameOf = relationName

-- | 'Relation's are equal modulo 'NodeId's whenever their components are.
instance EqModuloNodeId π => EqModuloNodeId (RelationLike π) where
  r === r' = relationName r === relationName r' && relationParams r === relationParams r'

-- | A relation declaration as produced by the parser.
type Relation = RelationLike Type

-- | An assumption within a rule body.
type Assumption = RelationLike SimpleIdentifier

-- | A conclusion of a rule.
--
--   This is 'Core.Relation' instantiated with 'Core.SimpleIdentifier'
--   for the relation name and @[Core.Expr]@ for the argument expressions.
--   The conclusion appears on the right side of the turnstile (|-)
--   and specifies what the rule derives.
--
--   Example: @|- Dist x y@
type Conclusion =
  RelationLike Expr
  -- ^ The types of the relation's arguments.

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
data Rule = Rule
  { ruleNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , ruleName :: Maybe SimpleIdentifier
  -- ^ The optional rule name (e.g. @myRule@, or @Nothing@ if unnamed).
  , ruleBoundVars :: [SimpleIdentifier]
  -- ^ The bound variables of the rule (e.g. @[x, y]@).
  , ruleAssumptions :: [Assumption]
  -- ^ The assumptions (relations applied to variables).
  , ruleConditions :: [Condition]
  -- ^ The conditions (expressions guarded by 'if').
  , ruleConclusion :: Conclusion
  -- ^ The conclusion (a relation applied to expressions).
  }
  deriving (Show, Eq)

-- | 'Rule's are equal modulo 'NodeId's whenever their components are.
instance EqModuloNodeId Rule where
  r === r' =
    ruleName r === ruleName r'
      && ruleBoundVars r === ruleBoundVars r'
      && ruleAssumptions r === ruleAssumptions r'
      && ruleConditions r === ruleConditions r'
      && ruleConclusion r === ruleConclusion r'

-- | A 'Rule' is its own node ID.
instance HasNodeId Rule where
  getNodeId = ruleNodeId

-- | A 'Rule' is named by its rule name.
instance Named Rule (Maybe SimpleIdentifier) where
  nameOf = ruleName

-- | A condition within a rule body.
--
--   Conditions are expressions guarded by the 'if' keyword. They evaluate
--   to a boolean truth value that must hold for the rule to fire.
--
--   Example: @if a <= b@
data Condition = Condition
  { conditionNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , conditionExpr :: Expr
  -- ^ The condition expression.
  }
  deriving (Show, Eq)

-- | A 'Condition' is its own node ID.
instance HasNodeId Condition where
  getNodeId = conditionNodeId

instance EqModuloNodeId Condition where
  Condition {conditionExpr = e} === Condition {conditionExpr = e'} = e === e'

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
data HsBlock = HsBlock
  { hsBlockNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , hsBlockContents :: Text
  -- ^ The Haskell source code contents.
  }
  deriving (Show, Eq)

-- | A 'HsBlock' is its own node ID.
instance HasNodeId HsBlock where
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
data HsImport = HsImport
  { hsImportNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , hsImportImport :: ModuleName
  -- ^ The imported module name (e.g. @Data.List@).
  }
  deriving (Show, Eq)

-- | A 'HsImport' is its own node ID.
instance HasNodeId HsImport where
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
data Include = Include
  { includeNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , includePath :: Text
  -- ^ The path to the included Fixen file.
  }
  deriving (Show, Eq)

-- | An 'Include' is its own node ID.
instance HasNodeId Include where
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
data Priority = Priority
  { priorityNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , priorityPremise :: Expr
  -- ^ The premise expression that must hold for the priority to apply.
  , priorityConclusion :: PriorityConclusion
  -- ^ The conclusion comparing two rule instances with an ordering.
  }
  deriving (Show, Eq)

-- | A 'Priority' is its own node ID.
instance HasNodeId Priority where
  getNodeId = priorityNodeId

-- | The conclusion of a priority declaration.
--
--   This compares two rule instances with an ordering symbol (@<=@ or
--   @⊏@), specifying that the left-hand side instance has lower or
--   equal priority than the right-hand side.
data PriorityConclusion = PriorityConclusion
  { priorityConclusionNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , priorityConclusionLHS :: RuleInstance
  -- ^ The left-hand side rule instance.
  , priorityConclusionRHS :: RuleInstance
  -- ^ The right-hand side rule instance.
  }
  deriving (Show, Eq)

-- | A 'PriorityConclusion' is its own node ID.
instance HasNodeId PriorityConclusion where
  getNodeId = priorityConclusionNodeId

-- | The instantiation of a rule, used in priority declarations.
--
--   A rule instance consists of a rule name and an optional map of
--   variable substitutions. The map describes how the rule's bound
--   variables are instantiated with specific values.
--
--   Example: @addDist { a = a, b = b' }@ instantiates the @addDist@
--   rule with @a@ mapping to @a@ and @b@ mapping to @b'@.
data RuleInstance = RuleInstance
  { ruleInstanceNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , ruleInstanceRule :: SimpleIdentifier
  -- ^ The rule being instantiated.
  , ruleInstanceMap :: Map SimpleIdentifier SimpleIdentifier
  -- ^ The variable substitution map.
  }
  deriving (Show, Eq)

-- | A 'RuleInstance' is its own node ID.
instance HasNodeId RuleInstance where
  getNodeId = ruleInstanceNodeId

-- | A 'RuleInstance' is named by its rule reference.
instance Named RuleInstance SimpleIdentifier where
  nameOf = ruleInstanceRule

-- | A relation that is queried.
--
-- This is 'Core.Relation' instantiated with:
--
-- * 'Core.SimpleIdentifier' for the relation name
-- * '[Core.QueryMode]' for the arguments
type QueriedRelation = RelationLike QueryMode

-- -- | A query declaration as produced by the parser.
-- --
-- --   This is 'Core.Query' instantiated with:
-- --
-- --   * 'QueriedRelation' for the queried relation
-- --   * 'Core.SimpleIdentifier' for the query name
-- --
-- --   Example: @query DistTo as distTo - +@
-- type Query = Core.Query QueriedRelation Core.SimpleIdentifier
--

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
data Query = Query
  { queryNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , queryRel :: QueriedRelation
  -- ^ The relation being queried.
  , queryName :: SimpleIdentifier
  -- ^ The query name (lowercase identifier for reference).
  }
  deriving (Show, Eq)

-- | A 'Query' is named by its query name.
instance Named Query SimpleIdentifier where
  nameOf = queryName

-- | A 'Query' is its own node ID.
instance HasNodeId Query where
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
data ModuleDeclaration = ModuleDeclaration
  { moduleDeclarationNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , moduleDeclarationName :: ModuleName
  -- ^ The Haskell module name being generated.
  }
  deriving (Show, Eq)

-- | A 'ModuleDeclaration' is its own node ID.
instance HasNodeId ModuleDeclaration where
  getNodeId = moduleDeclarationNodeId

-- | A 'ModuleDeclaration' is named by its module name.
instance Named ModuleDeclaration ModuleName where
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
data Extern = Extern
  { externNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , externSymbols :: NonEmpty SimpleIdentifier
  -- ^ The list of externally defined symbols.
  }
  deriving (Show, Eq)

-- | An 'Extern' is its own node ID.
instance HasNodeId Extern where
  getNodeId = externNodeId

-- type PartialOrdDeclaration = Core.PartialOrdDeclaration Core.SimpleIdentifier Core.Type Core.Identifier Core.Identifier

-- | Represents the @*@ wildcard in phase declarations.
--
--   In phase declarations, @*@ denotes "all remaining rules" — i.e., rules
--   that have not been assigned to any earlier phase. This newtype wraps
--   a 'NodeId' for source position tracking.
--
--   The wildcard can only appear as the last phase in a declaration.
--   After the symbol solver resolves all rule references, the rules
--   collected under this wildcard are those not explicitly named in
--   any earlier ruleset.
newtype EverythingElseRuleset
  = -- | The 'NodeId' for source position tracking.
    EverythingElseRuleset NodeId
  deriving (Show, Eq)

instance HasNodeId EverythingElseRuleset where
  -- \| Extract the 'NodeId' from an 'EverythingElseRuleset'.
  getNodeId (EverythingElseRuleset i) = i

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
data PartialOrdDeclaration = PartialOrdDeclaration
  { partialOrdDeclarationNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , partialOrdDeclarationName :: SimpleIdentifier
  -- ^ The type being defined as a partial order.
  , partialOrdDeclarationType :: Type
  -- ^ The base type of the partial order.
  , partialOrdDeclarationLeq :: Identifier
  -- ^ The less-than-or-equal comparison function.
  , partialOrdDeclarationMlbs :: Identifier
  -- ^ The maximal lower bounds function.
  }
  deriving (Show, Eq)

-- | A 'PartialOrdDeclaration' is its own node ID.
instance HasNodeId PartialOrdDeclaration where
  getNodeId = partialOrdDeclarationNodeId

-- | A 'PartialOrdDeclaration' is named by its type name.
instance Named PartialOrdDeclaration SimpleIdentifier where
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
data Phases = Phases
  { phasesNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , phasesPhases :: NonEmpty RulesetOrEverythingElse
  -- ^ The phase declarations (list of rulesets).
  }
  deriving (Show, Eq)

-- | A 'Phases' is its own node ID.
instance HasNodeId Phases where
  getNodeId = phasesNodeId

-- | A set of rule names, representing the contents of a phase declaration.
--
--   A ruleset is either an explicit list of rule names or the wildcard
--   @*@ (all remaining rules). Rulesets are used to group rules into
--   execution phases.
data Ruleset = Ruleset
  { ruleSetNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  , ruleSetRules :: NonEmpty SimpleIdentifier
  -- ^ The rule names in this ruleset.
  }
  deriving (Show, Eq)

-- | A 'Ruleset' is its own node ID.
instance HasNodeId Ruleset where
  getNodeId = ruleSetNodeId

-- | An explicit ruleset as produced by the parser.
--
--   This is 'Core.Ruleset' instantiated with a 'NonEmpty' list of
--   'Core.SimpleIdentifier' values — the named rules in the ruleset.
--   A ruleset represents the contents of a phase declaration.
--
--   Example: @rule1, rule2, rule3@ within a phase.
type ExplicitRuleset = Ruleset

--

-- | Either an explicit ruleset (a named list of rules) or the
--   @*@ wildcard ('EverythingElseRuleset') representing all remaining
--   rules.
--
--   This type appears in phase declarations where each phase is either
--   a named set of rules or the wildcard @*@ (which can only appear
--   as the last phase).
type RulesetOrEverythingElse = Either Ruleset EverythingElseRuleset

--

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
data Program = Program
  { moduleName :: ModuleDeclaration
  -- ^ The module declaration (e.g. @module My.Haskell.Module where@).
  , hsImports :: [HsImport]
  -- ^ Haskell module imports.
  , hsBlocks :: [HsBlock]
  -- ^ Embedded Haskell source code blocks.
  , includes :: [Include]
  -- ^ Included Fixen files.
  , relations :: [Relation]
  -- ^ Relation declarations.
  , partialOrdDeclarations :: [PartialOrdDeclaration]
  -- ^ Partial order declarations.
  , rules :: [Rule]
  -- ^ Rule declarations.
  , priorities :: [Priority]
  -- ^ Priority declarations.
  , queries :: [Query]
  -- ^ Query declarations.
  , phases :: Maybe Phases
  -- ^ Phase declarations (optional; 'Nothing' if no phases declared).
  }
  deriving (Show, Eq)

-- -- | An extern declaration as produced by the parser.
-- --
-- --   This is 'Core.Extern' instantiated with a 'NonEmpty' list of
-- --   'Core.SimpleIdentifier' values, listing symbols defined in
-- --   Haskell source code.
-- type Extern = Core.Extern (NonEmpty Core.SimpleIdentifier)
--
-- -- | A relation that is queried.
-- --
-- -- This is 'Core.Relation' instantiated with:
-- --
-- -- * 'Core.SimpleIdentifier' for the relation name
-- -- * '[Core.QueryMode]' for the arguments
-- type QueriedRelation = Core.Relation Core.SimpleIdentifier [Core.QueryMode]
--
-- -- | A query declaration as produced by the parser.
-- --
-- --   This is 'Core.Query' instantiated with:
-- --
-- --   * 'QueriedRelation' for the queried relation
-- --   * 'Core.SimpleIdentifier' for the query name
-- --
-- --   Example: @query DistTo as distTo - +@
-- type Query = Core.Query QueriedRelation Core.SimpleIdentifier
--
-- -- | A module declaration as produced by the parser.
-- --
-- --   This is 'Core.ModuleDeclaration' instantiated with 'Core.ModuleName'
-- --   for the generated Haskell module name.
-- --
-- --   Example: @module My.Haskell.Module where@
-- type ModuleDeclaration = Core.ModuleDeclaration Core.ModuleName
--
-- -- | A partial order declaration as produced by the parser.
-- --
-- --   This is 'Core.PartialOrdDeclaration' instantiated with:
-- --
-- --   * 'Core.SimpleIdentifier' for the type name
-- --   * 'Core.Type' for the base type
-- --   * 'Core.Identifier' for the less-than-or-equal function
-- --   * 'Core.Identifier' for the maximal lower bounds function
-- --
-- --   Example:
-- --
-- --   @
-- --   partial ord Dist
-- --       where
-- --       type = Dist
-- --       leq = (<=)
-- --       mlbs = meet
-- --   @
-- type PartialOrdDeclaration = Core.PartialOrdDeclaration Core.SimpleIdentifier Core.Type Core.Identifier Core.Identifier
--
-- -- | Represents the @*@ wildcard in phase declarations.
-- --
-- --   In phase declarations, @*@ denotes "all remaining rules" — i.e., rules
-- --   that have not been assigned to any earlier phase. This newtype wraps
-- --   a 'NodeId' for source position tracking.
-- --
-- --   The wildcard can only appear as the last phase in a declaration.
-- --   After the symbol solver resolves all rule references, the rules
-- --   collected under this wildcard are those not explicitly named in
-- --   any earlier ruleset.
-- newtype EverythingElseRuleset = EverythingElseRuleset NodeId
--   -- ^ The 'NodeId' for source position tracking.
--   deriving (Show, Eq)
--
-- instance HasNodeId EverythingElseRuleset where
--   -- | Extract the 'NodeId' from an 'EverythingElseRuleset'.
--   getNodeId (EverythingElseRuleset i) = i
--
-- -- | An explicit ruleset as produced by the parser.
-- --
-- --   This is 'Core.Ruleset' instantiated with a 'NonEmpty' list of
-- --   'Core.SimpleIdentifier' values — the named rules in the ruleset.
-- --   A ruleset represents the contents of a phase declaration.
-- --
-- --   Example: @rule1, rule2, rule3@ within a phase.
-- type ExplicitRuleset = Core.Ruleset (NonEmpty Core.SimpleIdentifier)
--
-- -- | Either an explicit ruleset (a named list of rules) or the
-- --   @*@ wildcard ('EverythingElseRuleset') representing all remaining
-- --   rules.
-- --
-- --   This type appears in phase declarations where each phase is either
-- --   a named set of rules or the wildcard @*@ (which can only appear
-- --   as the last phase).
-- type RulesetOrEverythingElse = Either ExplicitRuleset EverythingElseRuleset
--
-- -- | Phase declarations as produced by the parser.
-- --
-- --   This is 'Core.Phases' instantiated with a 'NonEmpty' list of
-- --   'RulesetOrEverythingElse' values. Phases organize rules into
-- --   execution order for multi-phase FPOP evaluation.
-- --
-- --   Example: @[{ rule1, rule2 }, { rule3 }, *]@
-- type Phases = Core.Phases (NonEmpty RulesetOrEverythingElse)
--
-- -- | A complete Fixen program as produced by the parser.
-- --
-- --   This is 'Core.Program' instantiated with all the concrete types
-- --   from this module. It represents the full AST of a Fixen program,
-- --   containing every construct from the module declaration through
-- --   queries and phases.
-- --
-- --   The fields are:
-- --
-- --   * 'moduleName' — the Haskell module declaration
-- --   * 'hsImports' — Haskell module imports
-- --   * 'hsBlocks' — embedded Haskell source code blocks
-- --   * 'includes' — included Fixen files
-- --   * 'extern' — externally defined symbols
-- --   * 'relations' — relation declarations
-- --   * 'partialOrdDeclarations' — partial order declarations
-- --   * 'rules' — rule declarations
-- --   * 'priorities' — priority declarations
-- --   * 'queries' — query declarations
-- --   * 'phases' — phase declarations (optional)
-- type Program =
--   Core.Program
--     ModuleDeclaration
--     -- ^ The module declaration (e.g. @module My.Haskell.Module where@).
--     [HsImport]
--     -- ^ Haskell module imports.
--     [HsBlock]
--     -- ^ Embedded Haskell source code blocks.
--     [Include]
--     -- ^ Included Fixen files.
--     [Extern]
--     -- ^ Externally defined symbols.
--     [Relation]
--     -- ^ Relation declarations.
--     [PartialOrdDeclaration]
--     -- ^ Partial order declarations.
--     [Rule]
--     -- ^ Rule declarations.
--     [Priority]
--     -- ^ Priority declarations.
--     [Query]
--     -- ^ Query declarations.
--     (Maybe Phases)
--     -- ^ Phase declarations (optional; 'Nothing' if no phases declared).

-------------------------------------------------------------------------------
-- Pretty-printing
-------------------------------------------------------------------------------

-- | Pretty-print a list of items as a vertical list with dash prefixes.
--
--   Each item is rendered on its own line, indented by 2 spaces and
--   prefixed with a dash (\"- \").
--
--   @
--   - item1
--   - item2
--   - item3
--   @
prettyList' :: [Doc ann] -> Doc ann
prettyList' items = vsep (formatItem <$> items)
  where
    formatItem item = hang 2 (pretty "-" <+> item)

-- | Format an identifier with its 'NodeId' as a compact @N suffix.
--
--   @
--   Dist@42
--   x@5
--   @
prettyId :: NodeId -> Text -> Doc ann
prettyId n name = pretty name <> pretty "@" <> pretty n

-- | Pretty-print an 'Expr' with syntax highlighting.
--
--   * Variables — plain text
--   * Integer literals — green
--   * String literals — yellow
--   * Operators (<=, <, +, -, *, etc.) — bold cyan
prettyExpr :: Expr -> Doc AnsiStyle
prettyExpr (ExprVar _ (MkIdentifierSimple _ n)) = pretty n
prettyExpr (ExprVar _ (MkIdentifierFQN _ _ n)) = pretty (fullIdentifier n)
prettyExpr (ExprApp _ f a) = lparen <> prettyExpr f <+> prettyExpr a <> rparen
prettyExpr (ExprIntLit _ i) = annotate (color Green) $ pretty i
prettyExpr (ExprStrLit _ s) = annotate (color Yellow) $ pretty (show s)
prettyExpr (ExprTuple _ f rs) =
  lparen
    <> prettyExpr f
    <> comma
    <> prettyExpr (NonEmpty.head rs)
    <> Prelude.foldr (\r acc -> comma <+> prettyExpr r <> acc) mempty (NonEmpty.toList rs)
    <> rparen
prettyExpr (ExprList _ ls) = lbracket <> sep (punctuate comma (prettyExpr <$> ls)) <> rbracket
prettyExpr (ExprUnit _) = lparen <> rparen

-- | Pretty-print an 'Assumption' (relation applied to variable names).
--
--   @
--   Dist@42 x, y
--   Edge@43 a, b, d
--   @
prettyAssumption :: Assumption -> Doc AnsiStyle
prettyAssumption (Relation _ name args) =
  annotate (color Red) (pretty (fullIdentifier name))
    <+> sep [pretty (fullIdentifier a) | a <- args]

-- | Pretty-print a 'Conclusion' (the |- part of a rule).
--
--   @
--   |- Dist@42 x (y + 1)
--   @
prettyConclusion :: Conclusion -> Doc AnsiStyle
prettyConclusion (Relation _ name args) =
  annotate bold (pretty "|-")
    <+> annotate (color Red) (pretty (fullIdentifier name))
    <+> sep (prettyExpr <$> args)

-- | Pretty-print a 'Condition' (an @if@ guard).
--
--   @
--   if x <= y
--   @
prettyCondition :: Condition -> Doc AnsiStyle
prettyCondition (Condition _ expr) =
  annotate bold (pretty "if") <+> prettyExpr expr

-- | Pretty-print a 'Rule' with structured, multi-line output.
--
--   @
--   rule addDist@34
--     boundVars: a, b, d, d'
--     assumptions:
--       Dist@34 a, d
--       Edge@34 a, b, d'
--     conditions:
--     conclusion:
--       |- Dist@35 b (d + d')
--   @
prettyRule :: Rule -> Doc AnsiStyle
prettyRule (Rule i name vars assumps conds concl) =
  let name_doc = case name of
        Nothing -> mempty
        Just n -> pretty " " <> annotate (color Red) (pretty (fullIdentifier n))
      vars_doc = pretty "boundVars:" <+> sep [pretty (fullIdentifier v) | v <- vars]
      assump_doc = pretty "assumptions:" <> line <> indent 2 (vsep (prettyAssumption <$> assumps))
      cond_doc = pretty "conditions:" <> line <> indent 2 (vsep (prettyCondition <$> conds))
      concl_doc = pretty "conclusion:" <> line <> indent 2 (prettyConclusion concl)
   in (name_doc <+> (pretty "(" <> pretty i <> pretty ")"))
        <> line
        <> indent 2 vars_doc
        <> line
        <> indent 2 assump_doc
        <> line
        <> indent 2 cond_doc
        <> line
        <> indent 2 concl_doc

-- | Pretty-print a 'RuleInstance' (rule with variable substitution map).
--
--   @
--   addDist@7 { d = d1, d' = d1' }
--   @
prettyRuleInstance :: RuleInstance -> Doc AnsiStyle
prettyRuleInstance (RuleInstance _ rule m) =
  if Map.null m
    then annotate (color Red) (pretty (fullIdentifier rule))
    else
      annotate (color Red) (pretty (fullIdentifier rule))
        <> pretty " {"
        <> sep (punctuate comma [pretty (fullIdentifier k) <> pretty " = " <> pretty (fullIdentifier v) | (k, v) <- Map.toAscList m])
        <> pretty " }"

-- | Pretty-print a 'PriorityConclusion' (lhs @op@ rhs).
--
--   @
--   addDist@7 <= addDist@8
--   @
prettyPriorityConclusion :: PriorityConclusion -> Doc AnsiStyle
prettyPriorityConclusion (PriorityConclusion _ lhs rhs) =
  prettyRuleInstance lhs
    <> pretty " "
    <> annotate bold (pretty "<=")
    <> pretty " "
    <> prettyRuleInstance rhs

-- | Pretty-print a 'Priority' declaration.
--
--   @
--   priority:
--     (d1 + d1') > (d2 + d2') |- addDist@7 <= addDist@8
--   @
prettyPriority :: Priority -> Doc AnsiStyle
prettyPriority (Priority _ premise concl) =
  annotate bold (pretty "priority:")
    <> line
    <> indent 2 (prettyExpr premise <> pretty " |- " <> prettyPriorityConclusion concl)

-- | Pretty-print a 'QueryMode' (@+@ for input, @-@ for output).
prettyQueryMode :: QueryMode -> Doc AnsiStyle
prettyQueryMode Input {} = annotate (color Green) (pretty "+")
prettyQueryMode Output {} = annotate (color Green) (pretty "-")

-- | Pretty-print a 'Query' declaration.
--
--   @
--   query distTo: DistTo@10 - +
--   @
prettyQuery :: Query -> Doc AnsiStyle
prettyQuery (Query _ rel qname) =
  annotate bold (pretty "query")
    <+> annotate (color Red) (pretty (fullIdentifier qname))
    <> pretty ": "
    <> annotate (color Red) (pretty (fullIdentifier $ nameOf rel))
    <+> sep (prettyQueryMode <$> (relationParams rel))

-- | Pretty-print a 'RulesetOrEverythingElse'.
--
--   @
--   { rule1@1, rule2@2 }
--   *
--   @
prettyRulesetOrEverythingElse :: RulesetOrEverythingElse -> Doc AnsiStyle
prettyRulesetOrEverythingElse (Left (Ruleset _ rules)) =
  lbrace <> sep (punctuate comma [pretty (fullIdentifier r) <> pretty "@" <> pretty (simpleIdentifierNodeId r) | r <- NonEmpty.toList rules]) <> rbrace
prettyRulesetOrEverythingElse (Right (EverythingElseRuleset n)) =
  annotate bold (pretty "*") <> pretty "@" <> pretty n

-- | Pretty-print 'Phases' declarations.
--
--   @
--   phases:
--     { rule1@1, rule2@2 }
--     { rule3@3 }
--     *@4
--   @
prettyPhases :: Phases -> Doc AnsiStyle
prettyPhases (Phases _ phases) =
  annotate bold (pretty "phases:")
    <> line
    <> indent 2 (vsep (prettyRulesetOrEverythingElse <$> NonEmpty.toList phases))

-- | Pretty-print an entire 'Program' with syntax highlighting.
--
--   Renders the complete Fixen program AST as a multi-line document
--   with colored, structured output. All sections are rendered with
--   blank-line separators between them. Sections with empty lists
--   are omitted entirely.
--
--   /Color scheme./
--
--   * Keywords (@module@, @rule@, @relation@, etc.) — bold blue
--   * Identifiers (relation names, rule names, variables) — red
--   * Types — red and bold
--   * Integer literals — green
--   * String literals — yellow
--   * Operators (@if@, @|-@, @<=@, @+@, @-@) — bold cyan
prettyProgram :: Program -> Doc AnsiStyle
prettyProgram
  Program
    { moduleName = module_name
    , hsImports = hs_imports
    , hsBlocks = hs_blocks
    , includes = p_includes
    , relations = p_relations
    , partialOrdDeclarations = partial_ords
    , rules = p_rules
    , priorities = p_priorities
    , queries = p_queries
    , phases = p_phases
    } =
    let mod_doc =
          annotate
            (color Green <> bold)
            ( pretty "module"
                <+> pretty (fullIdentifier (nameOf module_name))
            )

        imports_doc =
          if Prelude.null hs_imports
            then mempty
            else
              line
                <> annotate (color Blue <> bold) (pretty "imports")
                <> colon
                <> line
                <> indent 2 (prettyList' (prettyHsImport <$> hs_imports))

        hs_blocks_doc =
          if Prelude.null hs_blocks
            then mempty
            else
              line
                <> annotate (color Blue <> bold) (pretty "haskell source blocks")
                <> colon
                <> line
                <> indent 2 (prettyList' (prettyHsBlock <$> hs_blocks))

        include_doc =
          if Prelude.null p_includes
            then mempty
            else
              line
                <> annotate (color Blue <> bold) (pretty "includes")
                <> colon
                <> line
                <> indent 2 (prettyList' (prettyInclude <$> p_includes))

        relations_doc =
          if Prelude.null p_relations
            then mempty
            else
              line
                <> annotate (color Blue <> bold) (pretty "relations")
                <> colon
                <> line
                <> indent 2 (prettyList' (prettyRelation <$> p_relations))

        pord_doc =
          if Prelude.null partial_ords
            then mempty
            else
              line
                <> annotate (color Blue <> bold) (pretty "lattice declarations")
                <> colon
                <> line
                <> indent 2 (prettyList' (prettyPartialOrd <$> partial_ords))

        rules_doc =
          if Prelude.null p_rules
            then mempty
            else
              line
                <> annotate (color Blue <> bold) (pretty "rules")
                <> colon
                <> line
                <> indent 2 (prettyList' (prettyRule <$> p_rules))

        priorities_doc =
          if Prelude.null p_priorities
            then mempty
            else
              line
                <> annotate (color Blue <> bold) (pretty "priorities")
                <> colon
                <> line
                <> indent 2 (prettyList' (prettyPriority <$> p_priorities))

        queries_doc =
          if Prelude.null p_queries
            then mempty
            else
              line
                <> annotate (color Blue <> bold) (pretty "queries")
                <> colon
                <> line
                <> indent 2 (prettyList' (prettyQuery <$> p_queries))

        phases_doc =
          case p_phases of
            Nothing -> mempty
            Just p ->
              line
                <> annotate (color Blue <> bold) (pretty "phases")
                <> colon
                <> line
                <> indent 2 (prettyPhases p)
     in mod_doc
          <> imports_doc
          <> hs_blocks_doc
          <> include_doc
          <> relations_doc
          <> pord_doc
          <> rules_doc
          <> priorities_doc
          <> queries_doc
          <> phases_doc

-- | Pretty-print a 'HsImport' node.
--
--   Renders the imported module name followed by its 'NodeId' in
--   parentheses.
--
--   Output format: @Data.List (42)@
prettyHsImport :: HsImport -> Doc ann
prettyHsImport HsImport {hsImportNodeId = p, hsImportImport = module_name} =
  pretty (fullIdentifier module_name) <+> (lparen <> pretty p <> rparen)

-- | Pretty-print a 'HsBlock' node.
--
--   Renders the 'NodeId' in parentheses followed by the block contents
--   on the next line.
--
--   Output format:
--   @
--   (42)
--   myHaskellFunction :: Int -> Int
--   @
prettyHsBlock :: HsBlock -> Doc ann
prettyHsBlock (HsBlock p b) = lparen <> pretty p <> rparen <> line <> pretty b

-- | Pretty-print an 'Include' node.
--
--   Renders the file path followed by its 'NodeId' in parentheses.
--   Note: the field order is (path, nodeId) — the 'Include' constructor
--   stores the path first and the nodeId second.
--
--   Output format: @Path/To/Fixen.fix (17)@
prettyInclude :: Include -> Doc ann
prettyInclude (Include a p) = pretty p <+> (lparen <> pretty a <> rparen)

-- | Pretty-print a 'SimpleIdentifier' with its 'NodeId' annotation.
--
--   Renders the identifier name followed by its 'NodeId' in parentheses.
--   Used by 'prettyExtern' to display extern symbols.
--
--   Output format: @myFunction (99)@
prettySimpleIdentifierWithAnnotation :: SimpleIdentifier -> Doc ann
prettySimpleIdentifierWithAnnotation (SimpleIdentifier p s) = pretty s <+> (lparen <> pretty p <> rparen)

-- | Pretty-print a 'Type' with syntax highlighting.
--
--   Each type constructor is rendered with appropriate formatting:
--
--   * 'TypeName' — rendered in red and bold (e.g. @Integer@)
--   * 'TypeApp' — parenthesized application (e.g. @(Maybe Int)@)
--   * 'TypeList' — bracket notation (e.g. @[Int]@)
--   * 'TypeTuple' — parenthesized, comma-separated (e.g. @(Int, String)@)
--   * 'TypeUnit' — empty parentheses @()@
--   * 'TypeNatLit' — rendered in red (e.g. @42@)
--   * 'TypeSymbolLit' — rendered in yellow (e.g. @\"foo\"@)
prettyType :: Type -> Doc AnsiStyle

-- | A named type: render in red and bold.
prettyType (TypeName _ v) = annotate (color Red <> bold) $ pretty (fullIdentifier v)
-- \| Type application: parenthesize the LHS and RHS separated by space.
prettyType (TypeApp _ lhs rhs) = lparen <> prettyType lhs <+> prettyType rhs <> rparen
-- \| List type: wrap the element type in brackets.
prettyType (TypeList _ t) = lbracket <> prettyType t <> rbracket
-- \| Tuple type: enclose all elements (first + rest) in parentheses,
--   comma-separated.
prettyType (TypeTuple _ t ls) = encloseSep lparen rparen comma (prettyType <$> (t : NonEmpty.toList ls))
-- \| Unit type: render as empty parentheses.
prettyType (TypeUnit _) = lparen <> rparen
-- \| Natural number literal type: render in red.
prettyType (TypeNatLit _ i) = annotate (color Red) $ pretty i
-- \| Symbol (string) literal type: render in yellow.
prettyType (TypeSymbolLit _ s) = annotate (color Yellow) $ pretty (show s)

-- | Pretty-print a 'Relation' declaration with syntax highlighting.
--
--   Renders the keyword \"relation\" in bold, the relation name in
--   red and bold, the 'NodeId' in parentheses, and then the argument
--   types indented below with a \"types:\" label.
--
--   Output format:
--   @
--   relation Dist (42)
--     types:
--       - Integer
--       - Integer
--   @
prettyRelation :: Relation -> Doc AnsiStyle

-- | Render the \"relation\" keyword in bold, the name in red+bold,
--   and the nodeId in parentheses.
prettyRelation (Relation _ name args) =
  annotate (color Red <> bold) (pretty (fullIdentifier name))
    <> line
    -- \| Indent the \"types:\" label and the list of argument types.
    <> indent 2 (annotate (color Yellow) (pretty "types:") <> line <> indent 2 (prettyList' (prettyType <$> args)))

-- | Pretty-print a 'PartialOrdDeclaration' with syntax highlighting.
--
--   Renders the keyword \"partial ord\" in bold, the type name in
--   red and bold, the 'NodeId' in parentheses, and then three indented
--   lines showing the base type, the less-than-or-equal function,
--   and the maximal lower bounds function.
--
--   Output format:
--   @
--   partial ord Dist (42)
--     type : Dist
--     (⊑)  : (<=)
--     mlbs : meet
--   @
prettyPartialOrd :: PartialOrdDeclaration -> Doc AnsiStyle

-- | Render the \"partial ord\" keyword in bold, the name in red+bold,
--   and the nodeId in parentheses.
prettyPartialOrd (PartialOrdDeclaration p n t l m) =
  annotate bold (pretty "partial ord")
    <+> annotate (color Red <> bold) (pretty (fullIdentifier n))
    <+> (lparen <> pretty p <> rparen)
    <> line
    -- \| Indent the three fields: type, leq, and mlbs.
    <> indent
      2
      ( (annotate (color Yellow) (pretty "type ") <> colon <+> prettyType t)
          <> line
          -- \| Render the less-than-or-equal (⊑) function in red+bold.
          <> (annotate (color Yellow) (pretty "(⊑)  ") <> colon <+> (annotate (color Red <> bold) $ pretty (fullIdentifier l)))
          <> line
          -- \| Render the mlbs function name in red+bold.
          <> annotate (color Yellow) (pretty "mlbs ")
          <> colon
          <+> (annotate (color Red <> bold) $ pretty (fullIdentifier m))
      )

-- | Pretty-print an 'Extern' declaration with syntax highlighting.
--
--   Renders the keyword \"extern\" in blue and bold, followed by a
--   list of extern symbol names, each with its 'NodeId' annotation,
--   indented and dash-prefixed.
--
--   Output format:
--   @
--   extern:
--       - myFunction (1)
--       - anotherSymbol (2)
--   @
prettyExtern :: Extern -> Doc AnsiStyle

-- | Render the \"extern:\" label and the list of extern symbols
--   with their node IDs.
prettyExtern (Extern _ ls) =
  line <> annotate (color Blue <> bold) (pretty "extern") <> colon <> line <> indent 2 (prettyList' (prettySimpleIdentifierWithAnnotation <$> (NonEmpty.toList ls)))
