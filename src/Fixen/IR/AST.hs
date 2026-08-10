{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Fixen.IR.AST
-- Description : Abstract syntax trees
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module defines the data structures for the Abstract Syntax Tree
-- (AST) produced by the parser. The AST is then used for subsequent
-- compiler passes (symbol solving, type checking, code generation).
--
-- This module also provides a suite of pretty-printing functions using
-- the @prettyprinter@ library.
--
-- AST nodes also have lenses ("Control.Lens") that allow access and updates
-- to fields, see "Fixen.Fields".
--
-- @since 26.7
module Fixen.IR.AST where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text, append, cons, intercalate, unpack)
import Fixen.Fields hiding (cons)
import Fixen.Utils
import GHC.Natural
import Prettyprinter
import Prettyprinter.Render.Terminal

-------------------------------------------------------------------------------

-- * Node IDs

-- $nodeIds
--
-- Every construct in the AST is annotated with a unique, unchanging node ID.
-- The IDs are used to track information of each node, e.g., types, source
-- positions, etc.

-------------------------------------------------------------------------------

-- | A unique identifier assigned to every construct in the Fixen IR.
--
-- Internally this is represented as an 'Int'. Values are allocated
-- sequentially starting from 0 by the parser, so they are always
-- non-negative and strictly increasing within a single parse run.
--
-- @since 26.7
type NodeId = Int

-- | A class for comparing two AST nodes that are equal except for the node ID.
--
-- @since 26.7
class EqModuloNodeId α where
  -- | Equality modulo 'NodeId's.
  --
  -- @since 26.7
  (===) :: α -> α -> Bool
  a === b = (¬) (a /== b)

  -- | Disequality modulo 'NodeId's.
  --
  -- @since 26.7
  (/==) :: α -> α -> Bool
  a /== b = (¬) (a === b)

  {-# MINIMAL (===) | (/==) #-}

infix 4 ===
infix 4 /==

-- | Same as '(===)'
--
-- @since 26.7
(≅) :: EqModuloNodeId α => α -> α -> Bool
(≅) = (===)

infix 4 ≅
{-# INLINE (≅) #-}

-- | Same as '(/==)'
--
-- @since 26.7
(≇) :: EqModuloNodeId α => α -> α -> Bool
(≇) = (/==)

infix 4 ≇

{-# INLINE (≇) #-}

instance EqModuloNodeId α => EqModuloNodeId (Maybe α) where
  Nothing === Nothing = True
  Just x === Just y = x ≅ y
  _ === _ = False

instance EqModuloNodeId α => EqModuloNodeId [α] where
  [] === [] = True
  (x : xs) === (y : ys) = x ≅ y ∧ xs ≅ ys
  _ === _ = False

instance EqModuloNodeId α => EqModuloNodeId (NonEmpty α) where
  (x :| xs) === (y :| ys) = x ≅ y ∧ xs ≅ ys

-------------------------------------------------------------------------------

-- * Identifier types

-- $identifierTypes
--
-- We distinguish different kinds of identifiers in Fixen:
--
-- 1. 'SimpleIdentifier': The basic kind of identifier, e.g., @x@, @_myFunction@
--    and @:@.
-- 2. 'ModuleName': Haskell-style module names, e.g., @Control.Monad@
-- 3. 'FullyQualifiedName': Haskell-style fully qualified names, e.g.,
--    @Data.List.NonEmpty.cons@
-- 4. 'Identifier': Either a 'FullyQualifiedName' or a 'SimpleIdentifier'.
--
-- All these identifier types are captured by the 'IdentifierLike' class, which
-- allows us to convert identifiers into their 'Text' components, i.e., to
-- render these identifiers as 'Text'.

-------------------------------------------------------------------------------

-- ** The 'IdentifierLike' Class

-- | Class for types that represent identifiers in the program.
--
--   An 'IdentifierLike' value can be queried for:
--
--   * Its /simple/ name — the last component of the identifier
--     (e.g. @a@ from @My.Module.a@)
--   * Its /full/ name — the complete qualified name
--     (e.g. @My.Module.a@)
--
-- @since 26.7
class IdentifierLike σ where
  -- | Extract the simple (unqualified) name from an identifier.
  --
  --   For a fully-qualified name like @My.Module.a@, this returns @a@.
  --   For a simple identifier like @a@, this returns @a@ unchanged.
  --   For a 'ModuleName' like @Data.List@, this returns the entire
  --   module name as a single 'Text' value (@Data.List@).
  --
  -- @since 26.7
  simpleIdentifier :: σ -> Text

  -- | Extract the full (potentially qualified) name from an identifier.
  --
  --   For a fully-qualified name like @My.Module.a@, this returns
  --   @My.Module.a@. For a simple identifier like @a@, this returns
  --   @a@ unchanged.
  --
  -- @since 26.7
  fullIdentifier :: σ -> Text

-- ** (Unqualified) Identifiers

-- | A simple (unqualified) identifier in the program.
--
-- This wraps a 'Text' value with a 'NodeId'. Simple identifiers are used for
-- variable names, relation names, rule names, and the final component of
-- fully-qualified names.
--
-- Examples: @x@, @myFunction@, @Data@, @Just@
--
-- @since 26.7
data SimpleIdentifier = SimpleIdentifier
  { simpleIdentifierNodeId :: NodeId
  -- ^ The 'NodeId' attached to this 'SimpleIdentifier'.
  --
  -- @since 26.7
  , simpleIdentifierName :: Text
  -- ^ The identifier text (e.g. @x@, @myFunction@, @Data@).
  --
  -- @since 26.7
  }
  deriving (Show, Eq)

-- | 'SimpleIdentifier's are equal modulo 'NodeId's whenever their
-- texts are equal
--
-- @since 26.7
instance EqModuloNodeId SimpleIdentifier where
  a === b = simpleIdentifierName a == simpleIdentifierName b

-- | Simple identifiers are ordered lexicographically by their name.
--
-- @since 26.7
instance Ord SimpleIdentifier where
  SimpleIdentifier {simpleIdentifierName = n}
    <= SimpleIdentifier {simpleIdentifierName = n'} = n <= n'

instance HasNodeId SimpleIdentifier NodeId where
  nodeId = lens simpleIdentifierNodeId (\s i -> s {simpleIdentifierNodeId = i})

instance HasName SimpleIdentifier Text where
  name = lens simpleIdentifierName (\s i -> s {simpleIdentifierName = i})

-- | A 'SimpleIdentifier' is trivially 'IdentifierLike': both its simple
-- and full names are just its text value.
--
-- @since 26.7
instance IdentifierLike SimpleIdentifier where
  simpleIdentifier = simpleIdentifierName
  fullIdentifier = simpleIdentifierName

-- ** Module Names

-- | A Haskell-style module name, consisting of one or more capitalized
-- identifiers separated by dots.
--
-- Examples: @Data@, @Data.List@, @MyCompany.MyProject.MyModule@
--
-- @since 26.7
data ModuleName = ModuleName
  { moduleNodeId :: NodeId
  -- ^ The 'NodeId' attached to this 'ModuleName'.
  --
  -- @since 26.7
  , moduleNameName :: NonEmpty SimpleIdentifier
  -- ^ The individual components of the module name (e.g. @Data@ and
  -- @List@ for @Data.List@).
  --
  -- @since 26.7
  }
  deriving (Show, Eq)

instance HasNodeId ModuleName NodeId where
  nodeId = lens moduleNodeId (\s i -> s {moduleNodeId = i})

instance HasName ModuleName (NonEmpty SimpleIdentifier) where
  name = lens moduleNameName (\s i -> s {moduleNameName = i})

-- | A 'ModuleName' is 'IdentifierLike': its simple and full names are
-- both its components joined by dots.
--
-- @since 26.7
instance IdentifierLike ModuleName where
  simpleIdentifier m =
    intercalate
      "."
      ( m
          ^. name
          <&> simpleIdentifier
          & NonEmpty.toList
      )
  fullIdentifier = simpleIdentifier

-- | 'ModuleName's are equal modulo 'NodeIds' whenever their names are.
--
-- @since 26.7
instance EqModuloNodeId ModuleName where
  a === b = moduleNameName a ≅ moduleNameName b

-- ** Fully Qualified Names

-- | A fully-qualified name in the program, consisting of a 'ModuleName'
-- prefix and a final 'SimpleIdentifier'.
--
-- Examples: @Data.List.map@, @MyModule.MyType@
--
-- @since 26.7
data FullyQualifiedName = FullyQualifiedName
  { fqnNodeId :: NodeId
  -- ^ The 'NodeId' attached to this 'FullyQualifiedName'.
  --
  -- @since 26.7
  , fqnModuleName :: ModuleName
  -- ^ The module prefix (e.g. @Data.List@ in @Data.List.map@).
  --
  -- @since 26.7
  , fqnName :: SimpleIdentifier
  -- ^ The final name component (e.g. @map@ in @Data.List.map@).
  --
  -- @since 26.7
  }
  deriving (Show, Eq)

instance HasNodeId FullyQualifiedName NodeId where
  nodeId = lens fqnNodeId (\s i -> s {fqnNodeId = i})

instance HasModuleName FullyQualifiedName ModuleName where
  moduleName = lens fqnModuleName (\s i -> s {fqnModuleName = i})

-- | 'FullyQualifiedName's are equal modulo 'NodeId's whenever their
-- module names and names are.
--
-- @since 26.7
instance EqModuloNodeId FullyQualifiedName where
  a === b = fqnModuleName a ≅ fqnModuleName b ∧ fqnName a ≅ fqnName b

-- | A 'FullyQualifiedName' is 'IdentifierLike': its simple name is
-- the final component, and its full name is the module prefix joined
-- with the final component by a dot.
--
-- @since 26.7
instance IdentifierLike FullyQualifiedName where
  simpleIdentifier = simpleIdentifier . fqnName
  fullIdentifier n =
    append
      (fullIdentifier (fqnModuleName n))
      (cons '.' (simpleIdentifier (fqnName n)))

-- ** The Main Identifier Type

-- | An identifier in the program, which is either a simple identifier or a
-- fully-qualified name.
--
-- This is a sum type used in contexts where either form is valid, such as
-- expression variables.
--
-- @since 26.7
data Identifier
  = IdentifierSimpleIdentifier
      { identifierSimpleIdentifier :: SimpleIdentifier
      -- ^ The simple identifier value.
      --
      -- @since 26.7
      }
  | IdentifierFullyQualifiedName
      { identifierFQN :: FullyQualifiedName
      -- ^ The fully-qualified name value.
      --
      -- @since 26.7
      }
  deriving (Show, Eq)

-- | Pattern synonym for constructing a simple identifier.
--
-- @since 26.7
pattern MkIdentifierSimple
  :: NodeId
  -- ^ The 'NodeId' for the identifier
  --
  -- @since 26.7
  -> Text
  -- ^ The identifier text
  --
  -- @since 26.7
  -> Identifier
pattern MkIdentifierSimple uniq s = IdentifierSimpleIdentifier (SimpleIdentifier uniq s)

-- | Pattern synonym for constructing a fully-qualified identifier.
--
-- @since 26.7
pattern MkIdentifierFQN
  :: NodeId
  -- ^ The 'NodeId' for the identifier
  --
  -- @since 26.7
  -> ModuleName
  -- ^ The module name prefix
  --
  -- @since 26.7
  -> SimpleIdentifier
  -- ^ The final name component
  --
  -- @since 26.7
  -> Identifier
pattern MkIdentifierFQN uniq mod i = IdentifierFullyQualifiedName (FullyQualifiedName uniq mod i)

{-# COMPLETE MkIdentifierSimple, MkIdentifierFQN #-}

instance HasNodeId Identifier NodeId where
  nodeId =
    lens
      ( \case
          MkIdentifierSimple u _ -> u
          MkIdentifierFQN u _ _ -> u
      )
      ( \s i -> case s of
          MkIdentifierSimple _ t -> MkIdentifierSimple i t
          MkIdentifierFQN _ a b -> MkIdentifierFQN i a b
      )

-- | An 'Identifier' is 'IdentifierLike': delegates to the inner value's
-- implementation.
--
-- @since 26.7
instance IdentifierLike Identifier where
  simpleIdentifier (IdentifierSimpleIdentifier i) = simpleIdentifier i
  simpleIdentifier (IdentifierFullyQualifiedName fqn) = simpleIdentifier fqn
  fullIdentifier (IdentifierSimpleIdentifier i) = fullIdentifier i
  fullIdentifier (IdentifierFullyQualifiedName i) = fullIdentifier i

-- | 'Identifier's are equal modulo 'NodeIds' whenever their components are.
--
-- @since 26.7
instance EqModuloNodeId Identifier where
  MkIdentifierSimple _ s === MkIdentifierSimple _ t = s == t
  MkIdentifierFQN _ a b === MkIdentifierFQN _ c d = a ≅ c ∧ b ≅ d
  _ === _ = False

--------------------------------------------------------------------------------

-- * Expressions and Types

-- $expressionsAndTypes
--
-- Fixen contains a subset of valid Haskell expressions and types. These are
-- captured as the 'Expr' and 'Type' types.

--------------------------------------------------------------------------------

-- ** Expressions

-- | An expression in the Fixen language.
--
--   Expressions appear in rule conditions, priority premises, and
--   conclusion arguments. The type supports variables, application
--   (function application), integer and string literals, tuples,
--   lists, and the unit value.
data Expr
  = -- | A variable expression, referencing an identifier.
    --
    -- @since 26.7
    ExprVar
      NodeId
      -- ^ The 'NodeId'.
      --
      -- @since 26.7
      Identifier
      -- ^ The identifier being referenced.
      --
      -- @since 26.7
  | -- | A function application expression (@f x@).
    --
    -- @since 26.7
    ExprApp
      NodeId
      -- ^ The 'NodeId'.
      --
      -- @since 26.7
      Expr
      -- ^ The function being applied.
      --
      -- @since 26.7
      Expr
      -- ^ The argument.
      --
      -- @since 26.7
  | -- | An integer literal expression.
    --
    -- @since 26.7
    ExprIntLit
      NodeId
      -- ^ The 'NodeId'.
      --
      -- @since 26.7
      Integer
      -- ^ The integer value.
      --
      -- @since 26.7
  | -- | A string literal expression.
    --
    -- @since 26.7
    ExprStrLit
      NodeId
      -- ^ The 'NodeId'.
      --
      -- @since 26.7
      Text
      -- ^ The string value.
      --
      -- @since 26.7
  | -- | A tuple expression. The tuple must have at least two elements.
    --
    -- @since 26.7
    ExprTuple
      NodeId
      -- ^ The 'NodeId'.
      --
      -- @since 26.7
      Expr
      -- ^ The first element of the tuple.
      --
      -- @since 26.7
      (NonEmpty Expr)
      -- ^ The remaining elements of the tuple (non-empty, ensuring at least 2 total).
      --
      -- @since 26.7
  | -- | A list expression.
    --
    -- @since 26.7
    ExprList
      NodeId
      -- ^ The 'NodeId'.
      --
      -- @since 26.7
      [Expr]
      -- ^ The elements of the list (may be empty).
      --
      -- @since 26.7
  | -- | The unit value @()@.
    --
    -- @since 26.7
    ExprUnit
      NodeId
      -- ^ The 'NodeId'.
      --
      -- @since 26.7
  deriving (Show, Eq)

instance HasNodeId Expr NodeId where
  nodeId = lens __exprnodeId __exprsetNodeId
    where
      __exprnodeId (ExprVar u _) = u
      __exprnodeId (ExprApp u _ _) = u
      __exprnodeId (ExprIntLit u _) = u
      __exprnodeId (ExprStrLit u _) = u
      __exprnodeId (ExprTuple u _ _) = u
      __exprnodeId (ExprList u _) = u
      __exprnodeId (ExprUnit u) = u
      __exprsetNodeId (ExprVar _ a) i = ExprVar i a
      __exprsetNodeId (ExprApp _ a b) i = ExprApp i a b
      __exprsetNodeId (ExprIntLit _ a) i = ExprIntLit i a
      __exprsetNodeId (ExprStrLit _ a) i = ExprStrLit i a
      __exprsetNodeId (ExprTuple _ a b) i = ExprTuple i a b
      __exprsetNodeId (ExprList _ a) i = ExprList i a
      __exprsetNodeId (ExprUnit _) i = ExprUnit i

-- | 'Expr's are equal modulo 'NodeId's whenever their components are.
--
-- @since 26.7
instance EqModuloNodeId Expr where
  ExprVar _ v === ExprVar _ v' = v ≅ v'
  ExprApp _ f x === ExprApp _ f' x' = f ≅ f' ∧ x ≅ x'
  ExprIntLit _ i === ExprIntLit _ i' = i == i'
  ExprStrLit _ s === ExprStrLit _ s' = s == s'
  ExprTuple _ h t === ExprTuple _ h' t' = h ≅ h' ∧ t ≅ t'
  ExprList _ l === ExprList _ l' = l ≅ l'
  ExprUnit _ === ExprUnit _ = True
  _ === _ = False

-- ** Types

-- | A type in the Fixen language.
--
--   Types appear as parameters to relation declarations. The type system
--   supports named types, type application, built-in list and tuple types,
--   the unit type, and literal types (natural numbers and symbols).
--
-- @since 26.7
data Type
  = -- | A named type, referencing an identifier (e.g. @Int@, @MyType@).
    --
    -- @since 26.7
    TypeName
      NodeId
      -- ^ The 'NodeId'.
      --
      -- @since 26.7
      Identifier
      -- ^ The type name.
      --
      -- @since 26.7
  | -- | Type application (@T1 T2@), representing a type constructor applied
    -- to an argument type.
    --
    -- @since 26.7
    TypeApp
      NodeId
      -- ^ The 'NodeId'.
      --
      -- @since 26.7
      Type
      -- ^ The type constructor (LHS).
      --
      -- @since 26.7
      Type
      -- ^ The argument type (RHS).
      --
      -- @since 26.7
  | -- | A list type (@[a]@). This is a built-in type, for convenience in
    -- pretty-printing and code generation.
    --
    -- @since 26.7
    TypeList
      NodeId
      -- ^ The 'NodeId'.
      --
      -- @since 26.7
      Type
      -- ^ The element type.
      --
      -- @since 26.7
  | -- | A tuple type (@(a, b, ...)@). This is a built-in type. The tuple must
    -- have at least two elements.
    --
    -- @since 26.7
    TypeTuple
      NodeId
      -- ^ The 'NodeId'.
      --
      -- @since 26.7
      Type
      -- ^ The type of the first element.
      --
      -- @since 26.7
      (NonEmpty Type)
      -- ^ The types of the remaining elements (non-empty, ensuring at least 2 total).
      --
      -- @since 26.7
  | -- | The unit type @()@.
    --
    -- @since 26.7
    TypeUnit
      NodeId
      -- ^ The 'NodeId'.
      --
      -- @since 26.7
  | -- | A natural number literal type (e.g. @0@, @1@, @42@).
    --
    -- @since 26.7
    TypeNatLit
      NodeId
      -- ^ The 'NodeId'.
      --
      -- @since 26.7
      Natural
      -- ^ The natural number value.
      --
      -- @since 26.7
  | -- | A string\/symbol literal type (e.g. @"foo"@).
    --
    -- @since 26.7
    TypeSymbolLit
      NodeId
      -- ^ The 'NodeId'.
      --
      -- @since 26.7
      Text
      -- ^ The symbol/string value.
      --
      -- @since 26.7
  deriving (Show, Eq)

instance HasNodeId Type NodeId where
  nodeId = lens __typegetnodeId __typesetNodeId
    where
      __typegetnodeId (TypeName u _) = u
      __typegetnodeId (TypeApp u _ _) = u
      __typegetnodeId (TypeNatLit u _) = u
      __typegetnodeId (TypeSymbolLit u _) = u
      __typegetnodeId (TypeUnit u) = u
      __typegetnodeId (TypeTuple u _ _) = u
      __typegetnodeId (TypeList u _) = u
      __typesetNodeId (TypeName _ a) i = TypeName i a
      __typesetNodeId (TypeApp _ a b) i = TypeApp i a b
      __typesetNodeId (TypeNatLit _ a) i = TypeNatLit i a
      __typesetNodeId (TypeSymbolLit _ a) i = TypeSymbolLit i a
      __typesetNodeId (TypeUnit _) i = TypeUnit i
      __typesetNodeId (TypeTuple _ a b) i = TypeTuple i a b
      __typesetNodeId (TypeList _ a) i = TypeList i a

-- | 'Type's are equal modulo 'NodeId's whenever their components are.
--
-- @since 26.7
instance EqModuloNodeId Type where
  TypeName _ v === TypeName _ v' = v ≅ v'
  TypeApp _ f x === TypeApp _ f' x' = f ≅ f' ∧ x ≅ x'
  TypeNatLit _ i === TypeNatLit _ i' = i == i'
  TypeSymbolLit _ s === TypeSymbolLit _ s' = s == s'
  TypeTuple _ h t === TypeTuple _ h' t' = h ≅ h' ∧ t ≅ t'
  TypeList _ l === TypeList _ l' = l ≅ l'
  TypeUnit _ === TypeUnit _ = True
  _ === _ = False

--------------------------------------------------------------------------------

-- * Program constructs

--------------------------------------------------------------------------------

-- ** Relation-Like Constructs

-- $relationLikeConstructs
--
-- Several program constructs are broadly related to relations, e.g., relation
-- declaration, assumptions, conclusions, etc. These are defined here.

-- | A relation-like declaration in the program.
--
-- @since 26.7
data RelationLike π = RelationLike
  { relationLikeNodeId :: NodeId
  -- ^ The 'NodeId'.
  --
  -- @since 26.7
  , relationLikeName :: SimpleIdentifier
  -- ^ The relation name (e.g. @Dist@, @MyFact@).
  --
  -- @since 26.7
  , relationLikeArgs :: [π]
  -- ^ The arguments to this relation.
  --
  -- @since 26.7
  }
  deriving (Show, Eq)

instance HasNodeId (RelationLike π) NodeId where
  nodeId = lens relationLikeNodeId (\s i -> s {relationLikeNodeId = i})

instance HasName (RelationLike π) SimpleIdentifier where
  name = lens relationLikeName (\s i -> s {relationLikeName = i})

instance HasArgs (RelationLike π) [π] where
  args = lens relationLikeArgs (\s i -> s {relationLikeArgs = i})

{- FOURMOLU_DISABLE -}
-- | 'Relation's are equal modulo 'NodeId's whenever their components are.
--
-- @since 26.7
instance EqModuloNodeId π => EqModuloNodeId (RelationLike π) where
  r === r' =
    relationLikeName r ≅ relationLikeName r'
      ∧ relationLikeArgs r ≅ relationLikeArgs r'
{- FOURMOLU_ENABLE -}

-- | A relation declaration in the program.
--
-- Relations represent facts or predicates that can be assumed or concluded
-- in rules. They have a name (typically capitalized, as they are
-- constructor-like) and a list of parameter types.
--
-- Example:
--
-- @
-- rel Dist: Integer, Integer
-- @
--
-- @since 26.7
type RelationDeclaration = RelationLike Type

-- | Constructor and destructor for 'RelationDeclaration's.
--
-- @{-# COMPLETE RelationDeclaration #-}@
--
-- @since 26.7
pattern RelationDeclaration
  :: NodeId
  -- ^ The 'NodeId'.
  --
  -- @since 26.7
  -> SimpleIdentifier
  -- ^ The name of the relation being declared.
  --
  -- @since 26.7
  -> [Type]
  -- ^ The types of the arguments to the relation.
  --
  -- @since 26.7
  -> RelationDeclaration
pattern RelationDeclaration a b c = RelationLike a b c

{-# COMPLETE RelationDeclaration #-}

-- | An assumption within a rule body. The arguments to the relation symbol in
-- assumptions are just variables, i.e., 'SimpleIdentifier's.
--
-- @since 26.7
type Assumption = RelationLike SimpleIdentifier

-- | Constructor and destructor for 'Assumption's.
--
-- @{-# COMPLETE Assumption #-}@
--
-- @since 26.7
pattern Assumption
  :: NodeId
  -- ^ The 'NodeId'.
  --
  -- @since 26.7
  -> SimpleIdentifier
  -- ^ The name of the relation symbol.
  --
  -- @since 26.7
  -> [SimpleIdentifier]
  -- ^ The arguments to the relation symbol.
  --
  -- @since 26.7
  -> Assumption
pattern Assumption a b c = RelationLike a b c

{-# COMPLETE Assumption #-}

-- | A conclusion of a rule. The arguments to the relation symbol in conclusions
-- are expressions, i.e., 'Expr'.
--
-- @since 26.7
type Conclusion = RelationLike Expr

-- | Constructor and destructor for 'Conclusion's.
--
-- @{-# COMPLETE Conclusion #-}@
--
-- @since 26.7
pattern Conclusion
  :: NodeId
  -- ^ The 'NodeId'.
  --
  -- @since 26.7
  -> SimpleIdentifier
  -- ^ The name of the relation symbol.
  --
  -- @since 26.7
  -> [Expr]
  -- ^ The arguments to the relation symbol.
  --
  -- @since 26.7
  -> Conclusion
pattern Conclusion a b c = RelationLike a b c

{-# COMPLETE Conclusion #-}

-- ** Rules

-- | A rule declaration in the program.
--
-- Example:
--
-- @
-- rule myRule x y: MyFact x, if x < y |- Dist x y
-- @
--
-- @since 26.7
data Rule = Rule
  { ruleNodeId :: NodeId
  -- ^ The 'NodeId'.
  --
  -- @since 26.7
  , ruleName :: Maybe SimpleIdentifier
  -- ^ The optional rule name (e.g. @myRule@, or @Nothing@ if unnamed).
  --
  -- @since 26.7
  , ruleArgs :: [SimpleIdentifier]
  -- ^ The parameters of the rule.
  --
  -- @since 26.7
  , ruleAssumptions :: [Assumption]
  -- ^ The assumptions (relations applied to variables).
  --
  -- @since 26.7
  , ruleConditions :: [Condition]
  -- ^ The conditions (expressions guarded by 'if').
  --
  -- @since 26.7
  , ruleConclusion :: Conclusion
  -- ^ The conclusion (a relation applied to expressions).
  --
  -- @since 26.7
  }
  deriving (Show, Eq)

{- FOURMOLU_DISABLE -}
-- | 'Rule's are equal modulo 'NodeId's whenever their components are.
--
-- @since 26.7
instance EqModuloNodeId Rule where
  r === r' =
    ruleName r ≅ ruleName r'
      ∧ ruleArgs r ≅ ruleArgs r'
      ∧ ruleAssumptions r ≅ ruleAssumptions r'
      ∧ ruleConditions r ≅ ruleConditions r'
      ∧ ruleConclusion r ≅ ruleConclusion r'
{- FOURMOLU_ENABLE -}

instance HasNodeId Rule NodeId where
  nodeId = lens ruleNodeId (\s i -> s {ruleNodeId = i})

instance HasName Rule (Maybe SimpleIdentifier) where
  name = lens ruleName (\s i -> s {ruleName = i})

instance HasArgs Rule [SimpleIdentifier] where
  args = lens ruleArgs (\s i -> s {ruleArgs = i})

instance HasAssumptions Rule [Assumption] where
  assumptions = lens ruleAssumptions (\s i -> s {ruleAssumptions = i})

instance HasConditions Rule [Condition] where
  conditions = lens ruleConditions (\s i -> s {ruleConditions = i})

instance HasConclusion Rule Conclusion where
  conclusion = lens ruleConclusion (\s i -> s {ruleConclusion = i})

-- | A condition within a rule body.
--
--   Conditions are expressions guarded by the 'if' keyword. They evaluate
--   to a boolean truth value that must hold for the rule to fire.
--
--   Example: @if a <= b@
--
-- @since 26.7
data Condition = Condition
  { conditionNodeId :: NodeId
  -- ^ The 'NodeId'.
  --
  -- @since 26.7
  , conditionExpr :: Expr
  -- ^ The condition expression.
  --
  -- @since 26.7
  }
  deriving (Show, Eq)

instance HasNodeId Condition NodeId where
  nodeId = lens conditionNodeId (\s i -> s {conditionNodeId = i})

instance HasExpr Condition Expr where
  expr = lens conditionExpr (\s i -> s {conditionExpr = i})

instance EqModuloNodeId Condition where
  Condition {conditionExpr = e} === Condition {conditionExpr = e'} = e ≅ e'

-- ** Haskell Source Blocks

-- $hsSourceBlocks
--
-- Fixen allows users to write Haskell source blocks so as to keep their
-- Fixen-generated modules self-contained.

-- | A Haskell source code block embedded in a Fixen program.
--
-- Haskell blocks are delimited by triple backticks with the @hs@
-- language annotation (```hs ... ```). They allow defining Haskell
-- code that is referenced by 'extern' declarations.
--
-- Example:
--
-- @
-- ```hs
-- myHaskellFunction :: Int -> Int
-- myHaskellFunction x = x + 1
-- ```
-- @
--
-- @since 26.7
data HsBlock = HsBlock
  { hsBlockNodeId :: NodeId
  -- ^ The 'NodeId'.
  --
  -- @since 26.7
  , hsBlockContents :: Text
  -- ^ The Haskell source code contents.
  --
  -- @since 26.7
  }
  deriving (Show, Eq)

instance HasNodeId HsBlock NodeId where
  nodeId = lens hsBlockNodeId (\s i -> s {hsBlockNodeId = i})

instance HasContents HsBlock Text where
  contents = lens hsBlockContents (\s i -> s {hsBlockContents = i})

-- ** Imports

-- $imports
--
-- Users can also control what Haskell modules the Fixen-generated source code
-- imports. Note that these are __different__ to /includes/, which import
-- other Fixen programs.

-- | A Haskell module import statement.
--
-- Import statements allow a Fixen program to reference Haskell symbols
-- defined in external modules. The imported module is specified as a
-- 'ModuleName'.
--
-- Example:
--
-- @
-- import Data.List
-- @
--
-- @since 26.7
data HsImport = HsImport
  { hsImportNodeId :: NodeId
  -- ^ The 'NodeId'.
  --
  -- @since 26.7
  , hsImportModuleName :: ModuleName
  -- ^ The imported module name (e.g. @Data.List@).
  --
  -- @since 26.7
  , hsImportQualified :: Bool
  -- ^ Whether this is a qualified import
  --
  -- @since 26.7
  , hsImportAlias :: Maybe ModuleName
  -- ^ The alias for the import, if it exists
  --
  -- @since 26.7
  , hsImportSpecs :: Maybe Text
  -- ^ The explicit import specifications, if it exists.
  --
  -- @since 26.7
  }
  deriving (Show, Eq)

instance HasNodeId HsImport NodeId where
  nodeId = lens hsImportNodeId (\s i -> s {hsImportNodeId = i})

instance HasModuleName HsImport ModuleName where
  moduleName = lens hsImportModuleName (\s i -> s {hsImportModuleName = i})

-- ** Include Statements

-- $includes
--
-- Users can also __include__ other __Fixen__ programs into the current program.

-- | An @include@ statement for importing another Fixen program.
--
-- Include statements allow one Fixen program to reuse declarations
-- from another Fixen file. The included file path is a string literal.
--
-- Example:
--
-- @
-- include \"path\/to\/Program.fix\"
-- @
--
-- @since 26.7
data Include = Include
  { includeNodeId :: NodeId
  -- ^ The 'NodeId'.
  --
  -- @since 26.7
  , includePath :: Text
  -- ^ The path to the included Fixen file.
  --
  -- @since 26.7
  }
  deriving (Show, Eq)

instance HasNodeId Include NodeId where
  nodeId = lens includeNodeId (\s i -> s {includeNodeId = i})

instance HasPath Include Text where
  path = lens includePath (\s i -> s {includePath = i})

-- ** Priority Declarations

-- | A priority declaration.
--
-- Priority declarations specify an ordering between rule instances.
-- When multiple rules could fire, priorities determine which one
-- takes precedence. A priority consists of:
--
-- * A premise expression that must hold.
-- * A conclusion comparing two rule instances with an ordering symbol.
--
-- Example:
--
-- @
-- priority: a < b |- rule1 { a = a } < rule2 { b = b }
-- @
--
-- @since 26.7
data Priority = Priority
  { priorityNodeId :: NodeId
  -- ^ The 'NodeId'.
  --
  -- @since 26.7
  , priorityPremise :: Expr
  -- ^ The premise expression that must hold for the priority to apply.
  --
  -- @since 26.7
  , priorityConclusion :: PriorityConclusion
  -- ^ The conclusion comparing two rule instances with an ordering.
  --
  -- @since 26.7
  }
  deriving (Show, Eq)

instance HasNodeId Priority NodeId where
  nodeId = lens priorityNodeId (\s i -> s {priorityNodeId = i})

instance HasPremise Priority Expr where
  premise = lens priorityPremise (\s i -> s {priorityPremise = i})

instance HasConclusion Priority PriorityConclusion where
  conclusion = lens priorityConclusion (\s i -> s {priorityConclusion = i})

-- | The conclusion of a priority declaration.
--
--   This compares two rule instances with an ordering symbol (@<@ or
--   @⊏@), specifying that the left-hand side instance has lower
--   priority than the right-hand side.
--
-- @since 26.7
data PriorityConclusion = PriorityConclusion
  { priorityConclusionNodeId :: NodeId
  -- ^ The 'NodeId'.
  --
  -- @since 26.7
  , priorityConclusionLHS :: RuleInstance
  -- ^ The left-hand side rule instance. This rule instance is strictly less
  -- than the right-hand side rule instance.
  --
  -- @since 26.7
  , priorityConclusionRHS :: RuleInstance
  -- ^ The right-hand side rule instance. This rule instance is strictly
  -- greater than the left-hand side rule instance.
  --
  -- @since 26.7
  }
  deriving (Show, Eq)

instance HasNodeId PriorityConclusion NodeId where
  nodeId = lens priorityConclusionNodeId (\s i -> s {priorityConclusionNodeId = i})

instance HasLHS PriorityConclusion RuleInstance where
  lhs = lens priorityConclusionLHS (\s i -> s {priorityConclusionLHS = i})

instance HasRHS PriorityConclusion RuleInstance where
  rhs = lens priorityConclusionRHS (\s i -> s {priorityConclusionRHS = i})

-- | The instantiation of a rule, used in priority declarations.
--
--   A rule instance consists of a rule name and a potentially empty map of
--   variable substitutions. The map describes how the rule's parameters
--   are instantiated with specific values.
--
--   Example: @addDist { a = a, b = b' }@ instantiates the @addDist@
--   rule with @a@ mapping to @a@ and @b@ mapping to @b'@.
--
-- @since 26.7
data RuleInstance = RuleInstance
  { ruleInstanceNodeId :: NodeId
  -- ^ The 'NodeId'.
  --
  -- @since 26.7
  , ruleInstanceRule :: SimpleIdentifier
  -- ^ The name of the rule being instantiated.
  --
  -- @since 26.7
  , ruleInstanceMap :: Map SimpleIdentifier SimpleIdentifier
  -- ^ The variable substitution map.
  --
  -- @since 26.7
  }
  deriving (Show, Eq)

instance HasNodeId RuleInstance NodeId where
  nodeId = lens ruleInstanceNodeId (\s i -> s {ruleInstanceNodeId = i})

instance HasRule RuleInstance SimpleIdentifier where
  rule = lens ruleInstanceRule (\s i -> s {ruleInstanceRule = i})

instance HasMap RuleInstance (Map SimpleIdentifier SimpleIdentifier) where
  map = lens ruleInstanceMap (\s i -> s {ruleInstanceMap = i})

-- ** Query Declarations

-- | A relation that is queried. The arguments to this query declaration are
-- 'QueryMode's.
--
-- @since 26.7
type QueriedRelation = RelationLike QueryMode

-- | Constructor and destructor for 'QueriedRelation'.
--
-- @{-# COMPLETE QueriedRelation #-}@
--
-- @since 26.7
pattern QueriedRelation
  :: NodeId
  -- ^ The 'NodeID'.
  --
  -- @since 26.7
  -> SimpleIdentifier
  -- ^ The name of the relation symbol.
  --
  -- @since 26.7
  -> [QueryMode]
  -- ^ The arguments to the query.
  --
  -- @since 26.7
  -> QueriedRelation
pattern QueriedRelation a b c = RelationLike a b c

{-# COMPLETE QueriedRelation #-}

-- | A query declaration.
--
-- Query declarations specify how to query a relation, indicating which
-- arguments are inputs and which are outputs. Each query has:
--
-- * A relation name (capitalized)
-- * A query name (lowercase, for reference)
-- * A list of modes (@+@ for input, @-@ for output)
--
-- Example:
--
-- @
-- query distTo: DistTo - +
-- @
--
-- @since 26.7
data Query = Query
  { queryNodeId :: NodeId
  -- ^ The 'NodeId'.
  --
  -- @since 26.7
  , queryRel :: QueriedRelation
  -- ^ The relation being queried (and its modes).
  --
  -- @since 26.7
  , queryName :: SimpleIdentifier
  -- ^ The query name.
  --
  -- @since 26.7
  }
  deriving (Show, Eq)

instance HasName Query SimpleIdentifier where
  name = lens queryName (\s i -> s {queryName = i})

instance HasNodeId Query NodeId where
  nodeId = lens queryNodeId (\s i -> s {queryNodeId = i})

instance HasRelation Query QueriedRelation where
  relation = lens queryRel (\s i -> s {queryRel = i})

-- | The mode of a query argument: input (@+@) or output (@-@).
--
-- Query modes specify whether each argument of a relation is an input
-- (ground value to be matched) or an output (value to be computed).
--
-- * 'Input' — the argument is an input variable (ground)
-- * 'Output' — the argument is an output variable (to be computed)
--
-- @since 26.7
data QueryMode = Input NodeId | Output NodeId
  deriving (Show, Eq)

instance HasNodeId QueryMode NodeId where
  nodeId = lens __querymodenodeId __querymodesetNodeId
    where
      __querymodenodeId (Input u) = u
      __querymodenodeId (Output u) = u
      __querymodesetNodeId (Input _) i = Input i
      __querymodesetNodeId (Output _) i = Output i

-- ** Module Declaration

-- | A module declaration, specifying the Haskell module name that will
-- be generated from the Fixen program.
--
-- The module declaration is the first line of every Fixen program:
--
-- @
-- module My.Haskell.Module where
-- @
--
-- The module name determines the name of the generated Haskell module.
--
-- @since 26.7
data ModuleDeclaration = ModuleDeclaration
  { moduleDeclarationNodeId :: NodeId
  -- ^ The 'NodeId'.
  --
  -- @since 26.7
  , moduleDeclarationName :: ModuleName
  -- ^ The Haskell module name being generated.
  --
  -- @since 26.7
  }
  deriving (Show, Eq)

instance HasNodeId ModuleDeclaration NodeId where
  nodeId = lens moduleDeclarationNodeId (\s i -> s {moduleDeclarationNodeId = i})

instance HasName ModuleDeclaration ModuleName where
  name = lens moduleDeclarationName (\s i -> s {moduleDeclarationName = i})

instance HasModuleName ModuleDeclaration ModuleName where
  moduleName = lens moduleDeclarationName (\s i -> s {moduleDeclarationName = i})

-- ** Phases Declarations

-- | Represents the @*@ wildcard in phase declarations.
--
-- In phase declarations, @*@ denotes "all remaining rules"—i.e., rules
-- that have not been assigned to any earlier phase.
--
-- @since 26.7
newtype EverythingElseRuleset = EverythingElseRuleset NodeId
  deriving (Show, Eq)

instance HasNodeId EverythingElseRuleset NodeId where
  nodeId = lens __everythingElseRulesetGetNodeId __everythingElseRulesetSetNodeId
    where
      __everythingElseRulesetGetNodeId (EverythingElseRuleset i) = i
      __everythingElseRulesetSetNodeId (EverythingElseRuleset _) = EverythingElseRuleset

-- | A set of rule names, representing the contents of a phase declaration.
--
-- A ruleset is either an explicit list of rule names or the wildcard
-- @*@ (all remaining rules). Rulesets are used to group rules into
-- execution phases.
--
-- @since 26.7
data Ruleset = Ruleset
  { ruleSetNodeId :: NodeId
  -- ^ The 'NodeId' for source position tracking.
  --
  -- @since 26.7
  , ruleSetRules :: NonEmpty SimpleIdentifier
  -- ^ The rule names in this ruleset.
  --
  -- @since 26.7
  }
  deriving (Show, Eq)

instance HasNodeId Ruleset NodeId where
  nodeId = lens ruleSetNodeId (\s i -> s {ruleSetNodeId = i})

instance HasRules Ruleset (NonEmpty SimpleIdentifier) where
  rules = lens ruleSetRules (\s i -> s {ruleSetRules = i})

-- | Either an explicit ruleset (a named list of rules) or the
--   @*@ wildcard ('EverythingElseRuleset') representing all remaining
--   rules.
--
-- @since 26.7
type RulesetOrEverythingElse = Either Ruleset EverythingElseRuleset

-- | A phase declaration for multi-phase fixed-point computation.
--
-- Example:
--
-- @
-- phases: [ { rule1, rule2 }, { rule3 }, * ]
-- @
--
-- @since 26.7
data PhasesDeclaration = PhasesDeclaration
  { phasesNodeId :: NodeId
  -- ^ The 'NodeId'.
  --
  -- @since 26.7
  , phasesPhases :: NonEmpty RulesetOrEverythingElse
  -- ^ The phase declarations (list of rulesets).
  --
  -- @since 26.7
  }
  deriving (Show, Eq)

instance HasNodeId PhasesDeclaration NodeId where
  nodeId = lens phasesNodeId (\s i -> s {phasesNodeId = i})

instance HasPhases PhasesDeclaration (NonEmpty RulesetOrEverythingElse) where
  phases = lens phasesPhases (\s i -> s {phasesPhases = i})

-- ** Partial-Order Declarations

-- | A partial order declaration.
--
-- Partial order declarations inform Fixen how to:
--
-- * Compare elements of the type (via the 'leq' function)
-- * Compute maximal lower bounds of two elements (via the 'mlbs' function)
--
-- This is used by Fixen to generate optimized database representations.
--
-- Example:
--
-- @
-- partial ord Dist where
--   type = Dist
--   leq = (<=)
--   mlbs = meet
-- @
--
-- @since 26.7
data PartialOrdDeclaration = PartialOrdDeclaration
  { partialOrdDeclarationNodeId :: NodeId
  -- ^ The 'NodeId'.
  --
  -- @since 26.7
  , partialOrdDeclarationName :: SimpleIdentifier
  -- ^ The type being defined as a partial order.
  --
  -- @since 26.7
  , partialOrdDeclarationType :: Type
  -- ^ The base type of the partial order.
  --
  -- @since 26.7
  , partialOrdDeclarationLeq :: Identifier
  -- ^ The less-than-or-equal comparison function.
  --
  -- @since 26.7
  , partialOrdDeclarationMlbs :: Identifier
  -- ^ The maximal lower bounds function.
  --
  -- @since 26.7
  }
  deriving (Show, Eq)

instance HasNodeId PartialOrdDeclaration NodeId where
  nodeId = lens partialOrdDeclarationNodeId (\s i -> s {partialOrdDeclarationNodeId = i})

instance HasName PartialOrdDeclaration SimpleIdentifier where
  name = lens partialOrdDeclarationName (\s i -> s {partialOrdDeclarationName = i})

instance HasType PartialOrdDeclaration Type where
  ty = lens partialOrdDeclarationType (\s i -> s {partialOrdDeclarationType = i})

instance HasLeq PartialOrdDeclaration Identifier where
  leq = lens partialOrdDeclarationLeq (\s i -> s {partialOrdDeclarationLeq = i})

instance HasMLBs PartialOrdDeclaration Identifier where
  mlbs = lens partialOrdDeclarationMlbs (\s i -> s {partialOrdDeclarationMlbs = i})

-- ** Lattice Declarations

-- | A lattice declaration.
--
-- Lattice declarations inform Fixen how to:
--
-- * Compare elements of the type (via the 'leq' function)
-- * Join two elements of the type (via the @join@ function)
-- * Meet two elements of the type (via the @meet@ function)
--
-- This is used by Fixen to generate optimized database representations.
--
-- Example:
--
-- @
-- lat Dist where
--   type = Dist
--   leq = (<=)
--   join = joinDist
--   meet = meetDist
-- @
--
-- @since 26.7
data LatticeDeclaration = LatticeDeclaration
  { latticeDeclarationNodeId :: NodeId
  -- ^ The 'NodeId'.
  --
  -- @since 26.7
  , latticeDeclarationName :: SimpleIdentifier
  -- ^ The type being defined as a partial order.
  --
  -- @since 26.7
  , latticeDeclarationType :: Type
  -- ^ The base type of the partial order.
  --
  -- @since 26.7
  , latticeDeclarationLeq :: Identifier
  -- ^ The less-than-or-equal comparison function.
  --
  -- @since 26.7
  , latticeDeclarationJoin :: Identifier
  -- ^ The join function.
  --
  -- @since 26.7
  , latticeDeclarationMeet :: Identifier
  -- ^ The meet function.
  --
  -- @since 26.7
  , latticeDeclarationBot :: Identifier
  -- ^ The meet function.
  --
  -- @since 26.7
  }
  deriving (Show, Eq)

instance HasNodeId LatticeDeclaration NodeId where
  nodeId = lens latticeDeclarationNodeId (\s i -> s {latticeDeclarationNodeId = i})

instance HasName LatticeDeclaration SimpleIdentifier where
  name = lens latticeDeclarationName (\s i -> s {latticeDeclarationName = i})

instance HasType LatticeDeclaration Type where
  ty = lens latticeDeclarationType (\s i -> s {latticeDeclarationType = i})

instance HasLeq LatticeDeclaration Identifier where
  leq = lens latticeDeclarationLeq (\s i -> s {latticeDeclarationLeq = i})

instance HasJoin LatticeDeclaration Identifier where
  join = lens latticeDeclarationJoin (\s i -> s {latticeDeclarationJoin = i})

instance HasMeet LatticeDeclaration Identifier where
  meet = lens latticeDeclarationMeet (\s i -> s {latticeDeclarationMeet = i})

instance HasBot LatticeDeclaration Identifier where
  bot = lens latticeDeclarationBot (\s i -> s {latticeDeclarationBot = i})

-------------------------------------------------------------------------------

-- * Fixen Programs

-------------------------------------------------------------------------------

-- | A complete Fixen program.
--
--   The 'Program' type contains all parts of a Fixen program, from the module
--   declaration through queries and phases.
--
-- @since 26.7
data Program = Program
  { programModuleName :: ModuleDeclaration
  -- ^ The module declaration
  --
  -- @since 26.7
  , programImports :: [HsImport]
  -- ^ Haskell module imports.
  --
  -- @since 26.7
  , programHsBlocks :: [HsBlock]
  -- ^ Embedded Haskell source code blocks.
  --
  -- @since 26.7
  , programIncludes :: [Include]
  -- ^ Included Fixen files.
  --
  -- @since 26.7
  , programRelationDeclarations :: [RelationDeclaration]
  -- ^ Relation declarations.
  --
  -- @since 26.7
  , programPartialOrdDeclarations :: [PartialOrdDeclaration]
  -- ^ Partial order declarations.
  --
  -- @since 26.7
  , programLatticeDeclarations :: [LatticeDeclaration]
  -- ^ Partial order declarations.
  --
  -- @since 26.7
  , programRules :: [Rule]
  -- ^ Rule declarations.
  --
  -- @since 26.7
  , programPriorities :: [Priority]
  -- ^ Priority declarations.
  --
  -- @since 26.7
  , programQueries :: [Query]
  -- ^ Query declarations.
  --
  -- @since 26.7
  , programPhases :: Maybe PhasesDeclaration
  -- ^ Phase declarations (optional; 'Nothing' if no phases declared).
  --
  -- @since 26.7
  }
  deriving (Show, Eq)

instance HasModuleName Program ModuleDeclaration where
  moduleName = lens programModuleName (\s i -> s {programModuleName = i})

instance HasImports Program [HsImport] where
  imports = lens programImports (\s i -> s {programImports = i})

instance HasHsBlocks Program [HsBlock] where
  hsBlocks = lens programHsBlocks (\s i -> s {programHsBlocks = i})

instance HasIncludes Program [Include] where
  includes = lens programIncludes (\s i -> s {programIncludes = i})

instance HasRelationDeclarations Program [RelationDeclaration] where
  relationDeclarations = lens programRelationDeclarations (\s i -> s {programRelationDeclarations = i})

instance HasPartialOrdDeclarations Program [PartialOrdDeclaration] where
  partialOrdDeclarations = lens programPartialOrdDeclarations (\s i -> s {programPartialOrdDeclarations = i})

instance HasLatticeDeclarations Program [LatticeDeclaration] where
  latticeDeclarations = lens programLatticeDeclarations (\s i -> s {programLatticeDeclarations = i})

instance HasRules Program [Rule] where
  rules = lens programRules (\s i -> s {programRules = i})

instance HasPriorities Program [Priority] where
  priorities = lens programPriorities (\s i -> s {programPriorities = i})

instance HasQueries Program [Query] where
  queries = lens programQueries (\s i -> s {programQueries = i})

instance HasPhases Program (Maybe PhasesDeclaration) where
  phases = lens programPhases (\s i -> s {programPhases = i})

-------------------------------------------------------------------------------

-- * Pretty-printing

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
    formatItem item = hang 2 ("-" <+> item)

-- | Format an identifier with its 'NodeId' as a compact @N suffix.
--
--   @
--   Dist@42
--   x@5
--   @
prettyId :: NodeId -> Text -> Doc ann
prettyId n name' = pretty name' <> "@" <> pretty n

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
prettyAssumption (Assumption _ n a) =
  annotate (color Red) (pretty (fullIdentifier n))
    <+> sep [pretty (fullIdentifier a') | a' <- a]

-- | Pretty-print a 'Conclusion' (the |- part of a rule).
--
--   @
--   |- Dist@42 x (y + 1)
--   @
prettyConclusion :: Conclusion -> Doc AnsiStyle
prettyConclusion (Conclusion _ n a) =
  annotate bold "|-"
    <+> annotate (color Red) (pretty (fullIdentifier n))
    <+> sep (prettyExpr <$> a)

-- | Pretty-print a 'Condition' (an @if@ guard).
--
--   @
--   if x <= y
--   @
prettyCondition :: Condition -> Doc AnsiStyle
prettyCondition (Condition _ e) =
  annotate bold (pretty $ unpack "if") <+> prettyExpr e

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
prettyRule (Rule i n vars assumps conds concl) =
  let name_doc = case n of
        Nothing -> mempty
        Just n' -> " " <> annotate (color Red) (pretty (fullIdentifier n'))
      vars_doc = "boundVars:" <+> sep [pretty (fullIdentifier v) | v <- vars]
      assump_doc = "assumptions:" <> line <> indent 2 (vsep (prettyAssumption <$> assumps))
      cond_doc = "conditions:" <> line <> indent 2 (vsep (prettyCondition <$> conds))
      concl_doc = "conclusion:" <> line <> indent 2 (prettyConclusion concl)
   in (name_doc <+> ("(" <> pretty i <> ")"))
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
prettyRuleInstance (RuleInstance _ r m) =
  if Map.null m
    then annotate (color Red) (pretty (fullIdentifier r))
    else
      annotate (color Red) (pretty (fullIdentifier r))
        <> " {"
        <> sep (punctuate comma [pretty (fullIdentifier k) <> " = " <> pretty (fullIdentifier v) | (k, v) <- Map.toAscList m])
        <> " }"

-- | Pretty-print a 'PriorityConclusion' (lhs @op@ rhs).
--
--   @
--   addDist@7 <= addDist@8
--   @
prettyPriorityConclusion :: PriorityConclusion -> Doc AnsiStyle
prettyPriorityConclusion (PriorityConclusion _ l r) =
  prettyRuleInstance l
    <> " "
    <> annotate bold "<"
    <> " "
    <> prettyRuleInstance r

-- | Pretty-print a 'Priority' declaration.
--
--   @
--   priority:
--     (d1 + d1') > (d2 + d2') |- addDist@7 <= addDist@8
--   @
prettyPriority :: Priority -> Doc AnsiStyle
prettyPriority (Priority _ prem concl) =
  annotate bold "priority:"
    <> line
    <> indent 2 (prettyExpr prem <> " |- " <> prettyPriorityConclusion concl)

-- | Pretty-print a 'QueryMode' (@+@ for input, @-@ for output).
prettyQueryMode :: QueryMode -> Doc AnsiStyle
prettyQueryMode Input {} = annotate (color Green) "+"
prettyQueryMode Output {} = annotate (color Green) "-"

-- | Pretty-print a 'Query' declaration.
--
--   @
--   query distTo: DistTo@10 - +
--   @
prettyQuery :: Query -> Doc AnsiStyle
prettyQuery (Query _ rel qname) =
  annotate bold "query"
    <+> annotate (color Red) (pretty (fullIdentifier qname))
    <> ": "
    <> annotate (color Red) (pretty (fullIdentifier $ rel ^. name))
    <+> sep (prettyQueryMode <$> (rel ^. args))

-- | Pretty-print a 'RulesetOrEverythingElse'.
--
--   @
--   { rule1@1, rule2@2 }
--   *
--   @
prettyRulesetOrEverythingElse :: RulesetOrEverythingElse -> Doc AnsiStyle
prettyRulesetOrEverythingElse (Left (Ruleset _ rules')) =
  lbrace <> sep (punctuate comma [pretty (fullIdentifier r) <> "@" <> pretty (simpleIdentifierNodeId r) | r <- NonEmpty.toList rules']) <> rbrace
prettyRulesetOrEverythingElse (Right (EverythingElseRuleset n)) =
  annotate bold "*" <> "@" <> pretty n

-- | Pretty-print 'Phases' declarations.
--
--   @
--   phases:
--     { rule1@1, rule2@2 }
--     { rule3@3 }
--     *@4
--   @
prettyPhases :: PhasesDeclaration -> Doc AnsiStyle
prettyPhases (PhasesDeclaration _ phases') =
  annotate bold "phases:"
    <> line
    <> indent 2 (vsep (prettyRulesetOrEverythingElse <$> NonEmpty.toList phases'))

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
    { programModuleName = module_name
    , programImports = hs_imports
    , programHsBlocks = hs_blocks
    , programIncludes = p_includes
    , programRelationDeclarations = p_relations
    , programPartialOrdDeclarations = partial_ords
    , programRules = p_rules
    , programPriorities = p_priorities
    , programQueries = p_queries
    , programPhases = p_phases
    } =
    let mod_doc =
          annotate
            (color Green <> bold)
            ( "module"
                <+> pretty (fullIdentifier (module_name ^. moduleName))
            )

        imports_doc =
          if Prelude.null hs_imports
            then mempty
            else
              line
                <> annotate (color Blue <> bold) "imports"
                <> colon
                <> line
                <> indent 2 (prettyList' (prettyHsImport <$> hs_imports))

        hs_blocks_doc =
          if Prelude.null hs_blocks
            then mempty
            else
              line
                <> annotate (color Blue <> bold) "haskell source blocks"
                <> colon
                <> line
                <> indent 2 (prettyList' (prettyHsBlock <$> hs_blocks))

        include_doc =
          if Prelude.null p_includes
            then mempty
            else
              line
                <> annotate (color Blue <> bold) "includes"
                <> colon
                <> line
                <> indent 2 (prettyList' (prettyInclude <$> p_includes))

        relations_doc =
          if Prelude.null p_relations
            then mempty
            else
              line
                <> annotate
                  (color Blue <> bold)
                  ( pretty $
                      unpack "relations"
                  )
                <> colon
                <> line
                <> indent 2 (prettyList' (prettyRelation <$> p_relations))

        pord_doc =
          if Prelude.null partial_ords
            then mempty
            else
              line
                <> annotate (color Blue <> bold) "lattice declarations"
                <> colon
                <> line
                <> indent 2 (prettyList' (prettyPartialOrd <$> partial_ords))

        rules_doc =
          if Prelude.null p_rules
            then mempty
            else
              line
                <> annotate (color Blue <> bold) "rules"
                <> colon
                <> line
                <> indent 2 (prettyList' (prettyRule <$> p_rules))

        priorities_doc =
          if Prelude.null p_priorities
            then mempty
            else
              line
                <> annotate (color Blue <> bold) "priorities"
                <> colon
                <> line
                <> indent 2 (prettyList' (prettyPriority <$> p_priorities))

        queries_doc =
          if Prelude.null p_queries
            then mempty
            else
              line
                <> annotate (color Blue <> bold) "queries"
                <> colon
                <> line
                <> indent 2 (prettyList' (prettyQuery <$> p_queries))

        phases_doc =
          case p_phases of
            Nothing -> mempty
            Just p ->
              line
                <> annotate (color Blue <> bold) "phases"
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
--   Renders the imported module name, optional import modifiers, optional
--   explicit import specifications, and the import's 'NodeId' in parentheses.
--
--   Output format: @qualified Data.Map as Map (Map) (42)@
prettyHsImport :: HsImport -> Doc ann
prettyHsImport HsImport {hsImportNodeId = p, hsImportQualified = q, hsImportModuleName = module_name, hsImportAlias = alias, hsImportSpecs = specs} =
  let qualified_doc =
        if q then pretty ("qualified " :: Text) else mempty
      alias_doc =
        case alias of
          Nothing -> mempty
          Just a -> pretty (" as " :: Text) <> pretty (fullIdentifier a)
      specs_doc =
        case specs of
          Nothing -> mempty
          Just s -> pretty (" " :: Text) <> pretty s
   in qualified_doc
        <> pretty (fullIdentifier module_name)
        <> alias_doc
        <> specs_doc
        <+> (lparen <> pretty p <> rparen)

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
prettyType (TypeApp _ l r) = lparen <> prettyType l <+> prettyType r <> rparen
prettyType (TypeList _ t) = lbracket <> prettyType t <> rbracket
prettyType (TypeTuple _ t ls) = encloseSep lparen rparen comma (prettyType <$> (t : NonEmpty.toList ls))
prettyType (TypeUnit _) = lparen <> rparen
prettyType (TypeNatLit _ i) = annotate (color Red) $ pretty i
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
prettyRelation :: RelationDeclaration -> Doc AnsiStyle

-- | Render the \"relation\" keyword in bold, the name in red+bold,
--   and the nodeId in parentheses.
prettyRelation (RelationDeclaration _ n a) =
  annotate (color Red <> bold) (pretty (fullIdentifier n))
    <> line
    <> indent 2 (annotate (color Yellow) "types:" <> line <> indent 2 (prettyList' (prettyType <$> a)))

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
  annotate bold "partial ord"
    <+> annotate (color Red <> bold) (pretty (fullIdentifier n))
    <+> (lparen <> pretty p <> rparen)
    <> line
    -- \| Indent the three fields: type, leq, and mlbs.
    <> indent
      2
      ( (annotate (color Yellow) "type " <> colon <+> prettyType t)
          <> line
          -- \| Render the less-than-or-equal (⊑) function in red+bold.
          <> (annotate (color Yellow) "(⊑)  " <> colon <+> (annotate (color Red <> bold) $ pretty (fullIdentifier l)))
          <> line
          -- \| Render the mlbs function name in red+bold.
          <> annotate (color Yellow) "mlbs "
          <> colon
          <+> (annotate (color Red <> bold) $ pretty (fullIdentifier m))
      )
