{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--     Module      : Fixen.IR.Core
--     Description : Building blocks for Fixen intermediate representations
--                   (IRs)
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     Building blocks for intermediate representations for Fixen. This
--     module uses hardcore abstractions and data types a la carte.
module Fixen.IR.Core (
  -- * Building blocks
  -- $buildingblocks

  -- ** Basics
  CoreItem (..),
  CorePair (..),
  CoreDouble (..),

  -- ** Common Program Constructs
  -- $programconstructs
  SimpleIdentifier (..),
  ModuleName (..),
  FullyQualifiedName (..),
  Identifier (.., MkIdentifierSimple, MkIdentifierFQN),
  IdentifierLike (..),
  Expr (..),
  Type (..),
  Relation (..),
  Rule (..),
  Condition (..),
  Fact (..),
  HsBlock (..),
  HsImport (..),
  Include (..),
  Priority (..),
  PriorityConclusion (..),
  RuleInstantiation (..),
  Query (..),
  ModuleDeclaration (..),
  PartialOrdDeclaration (..),
  Phases (..),
  Extern (..),
  Mode (..),
  Named (..),
  Ruleset (..),
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Typeable
import Fixen.IR.Core.Annotations
import GHC.TypeLits (KnownSymbol, Symbol)

-------------------------------------------------------------------------------
--
-- Building blocks

-- $buildingblocks
-- Here we define the basic building blocks of Fixen intermediate
-- representations. The reason we want to do this is so that common patterns
-- like annotated items, pairs and recursive types are captured all in the same
-- type to be re-used and for common operations to be easily defined.
--
-- You know things get serious when we use greek symbols for type parameters!
-- The core types use clever tricks on types to compose and yet be
-- differentiated easily. In particular, they all have a name 'Symbol' kind
-- which allows two disparate types built from the same building blocks to
-- still be distinguishable at the type level. The type parameters of the
-- constructors are also explicitly ordered so that you can apply the correct
-- symbol to specialize them.
--
-- See each type here for more information on usage.

-------------------------------------------------------------------------------

-- | Describes an annotated item. The type parameters are:
--
--  [@ℓ@] A name used to disambiguate multiple occurrences of
--        'CoreItem' that occur in a sum.
--  [@μ@] The type of the annotation.
--  [@a@] The type of the item being annotated.
--  [@ρ@] The recursor.
--
--  These are used as building blocks of larger types. For instance,
--  term-level identifiers can be expressed as the type
--
--  @type Identifier = 'CoreItem' "identifier" 'Position' 'String' 'Void'@
--
--  which simply annotates
--  a string with a source position ('Void' disables recursion). It can also be
--  used in recursive types by using 'Fixpoint' and not applying the last
--  type parameter.
--
--  To construct a 'CoreItem' with a specific name, just apply it to the
--  type symbol. For instance, if you want to use the 'CoreItem' constructor
--  to construct a @'CoreItem' "identifier" ...@, you can write:
--
--  @'CoreItem' \@"identifier" ...@
data CoreItem (ℓ :: Symbol) μ α (ρ :: κ) where
  CoreItem
    :: forall name ann a rec
     . !ann
    -- ^ The annotation
    -> !a
    -- ^ The annotated term
    -> CoreItem name ann a rec
  deriving (Eq, Typeable)

instance (KnownSymbol a, Show b, Show c) => Show (CoreItem a b c d) where
  show (CoreItem b c) =
    let type_name = show $ typeRep (Proxy :: Proxy a)
    in  "(CoreItem @" ++ drop 1 (init type_name) ++ " (" ++ show b ++ ") (" ++ show c ++ "))"

-- | Describes an annotated pair. The type parameters are:
--
--  [@ℓ@] A name used to disambiguate multiple occurrences of
--        'CorePair' that occur in a sum.
--  [@μ@] The type of the annotation.
--  [@α@] The type of the left projection
--  [@β@] The type of the right projection.
--
--  These are used as building blocks of pairs. For instance,
--  an @('Int', 'Bool')@ pair annotated with a 'String' can be expressed as
--
--  @type MyPair = 'CorePair' "p" 'String' 'Int' 'Bool'@
--
--  For recursive pairs (where both projections are the same recursive type),
--  see 'CoreDouble'.
--
--  To construct a 'CorePair' with a specific name, just apply it to the
--  type symbol. For instance, if you want to use the 'CorePair' constructor
--  to construct a @'CorePair' "identifier" ...@, you can write:
--
--  @'CorePair' \@"identifier" ...@
data CorePair (ℓ :: Symbol) μ α β where
  CorePair
    :: forall name ann left right
     . !ann
    -- ^ The annotation
    -> !left
    -- ^ The left projection
    -> !right
    -- ^ The right projection
    -> CorePair name ann left right
  deriving (Show, Eq)

-- | Describes an annotated pair whose projections are of the same type. The
-- type parameters are:
--
--  [@ℓ@] A name used to disambiguate multiple occurrences of
--        'CorePair' that occur in a sum.
--  [@μ@] The type of the annotation.
--  [@ρ@] The type of the projections.
--
--  These are used as building blocks of pairs with the same type. For instance,
--  an 'Int' pair annotated with a 'String' can be expressed as
--
--  @type MyPair = 'CoreDouble' "p" 'String' 'Int'@
--
--  The projections type can be recursive when built with 'Fixpoint' and by not
--  applying the last type parameter.
--
--  To construct a 'CoreDouble' with a specific name, just apply it to the
--  type symbol. For instance, if you want to use the 'CoreDouble' constructor
--  to construct a @'CoreDouble' "identifier" ...@, you can write:
--
--  @'CoreDouble' \@"identifier" ...@
--
--  The reason we do not re-use 'CorePair' and specialize it to one type
--  argument using a type synonym is because the type-checker can become unable
--  to terminate in those scenarios.
data CoreDouble (ℓ :: Symbol) μ ρ where
  CoreDouble
    :: forall name ann e
     . !ann
    -- ^ The annotation
    -> !e
    -- ^ The left projection
    -> !e
    -- ^ The right projection
    -> CoreDouble name ann e
  deriving (Show, Eq)

-------------------------------------------------------------------------------
--
-- Common program constructs

-- $programconstructs
-- Many parts of the intermediate representation have constructs that are
-- frequently occurring. We define them here.

-------------------------------------------------------------------------------

-- | An identifier in the program
data SimpleIdentifier α σ where
  -- | A simple identifier
  SimpleIdentifier
    :: forall ann name
     . ann
    -- ^ The annotation
    -> name
    -- ^ The name
    -> SimpleIdentifier ann name
  deriving (Show, Eq)

instance (Eq a, Ord t) => Ord (SimpleIdentifier a t) where
  SimpleIdentifier _ t <= SimpleIdentifier _ t' = t <= t'

-- | The name of a module
data ModuleName α ν where
  -- | A module name
  ModuleName
    :: forall ann name
     . ann
    -- ^ The annotation
    -> name
    -- ^ The name
    -> ModuleName ann name
  deriving (Show, Eq)

-- | A fully-qualified name in the program
data FullyQualifiedName α μ ν where
  -- | A fully-qualified name
  FullyQualifiedName
    :: forall ann mod name
     . ann
    -- ^ The annotation
    -> mod
    -- ^ The module
    -> name
    -- ^ The name
    -> FullyQualifiedName ann mod name
  deriving (Show, Eq)

data Identifier α β χ δ where
  -- | A simple identifier
  IdentifierSimpleIdentifier
    :: forall ann mod fqn sim
     . SimpleIdentifier ann sim
    -- ^ The simple identifier
    -> Identifier ann mod fqn sim
  -- | A fully-qualified name
  IdentifierFullyQualifiedName
    :: forall ann mod fqn sim
     . FullyQualifiedName ann mod fqn
    -- ^ The fully qualified name
    -> Identifier ann mod fqn sim
  deriving (Show, Eq)

-- | Pattern synonyms for easier construction of identifiers from simple identifiers.
pattern MkIdentifierSimple
  :: forall ann mod fqn sim
   . ann
  -- ^ The annotation
  -> sim
  -- ^ The simple identifier
  -> Identifier ann mod fqn sim
pattern MkIdentifierSimple ann s = IdentifierSimpleIdentifier (SimpleIdentifier ann s)

-- | Pattern synonyms for easier construction of identifiers from fully-qualified names.
pattern MkIdentifierFQN
  :: forall ann mod fqn sim
   . ann
  -- ^ The annotation
  -> mod
  -- ^ The module
  -> fqn
  -- ^ The name
  -> Identifier ann mod fqn sim
pattern MkIdentifierFQN ann mod i = IdentifierFullyQualifiedName (FullyQualifiedName ann mod i)

{-# COMPLETE MkIdentifierSimple, MkIdentifierFQN #-}

-- | A class for things that are identifier-like, i.e. they have an identifier part.
class IdentifierLike ɩ σ | ɩ -> σ where
  -- | Get the identifier part of the thing
  identifier
    :: ɩ
    -- ^ The thing
    -> σ
    -- ^ The identifier part

instance IdentifierLike (SimpleIdentifier α σ) σ where
  identifier (SimpleIdentifier _ n) = n

instance IdentifierLike (FullyQualifiedName α μ ν) ν where
  identifier (FullyQualifiedName _ _ n) = n

instance IdentifierLike β δ => IdentifierLike (Identifier α χ β δ) δ where
  identifier (MkIdentifierSimple _ n) = n
  identifier (MkIdentifierFQN _ _ n) = identifier n

data Expr α θ ι σ where
  -- | A name/variable expression
  ExprVar
    :: forall ann v int str
     . ann
    -- ^ The annotation
    -> v
    -- ^ The variable itself
    -> Expr ann v int str
  -- | An application expression
  ExprApp
    :: forall ann v int str
     . ann
    -- ^ The annotation
    -> (Expr ann v int str)
    -- ^ The expression being applied
    -> (Expr ann v int str)
    -- ^ The RHS of the application
    -> Expr ann v int str
  -- | An integer literal expression
  ExprIntLit
    :: forall ann v int str
     . ann
    -- ^ The annotation
    -> int
    -- ^ The integer literal
    -> Expr ann v int str
  -- | A string literal expression
  ExprStrLit
    :: forall ann v int str
     . ann
    -- ^ The annotation
    -> str
    -- ^ The string
    -> Expr ann v int str
  -- | A tuple expression. The tuple must have at least two elements.
  ExprTuple
    :: forall ann v int str
     . ann
    -- ^ The annotation
    -> Expr ann v int str
    -- ^ The first projection of the tuple
    -> NonEmpty (Expr ann v int str)
    -- ^ The other elements of the tuple
    -> Expr ann v int str
  -- | A list expression
  ExprList
    :: forall ann v int str
     . ann
    -- ^ The annotation
    -> [Expr ann v int str]
    -- ^ The elements of the list
    -> Expr ann v int str
  -- | The unit value ()
  ExprUnit
    :: forall ann v int str
     . ann
    -- ^ The annotation
    -> Expr ann v int str
    -- ^ The unit value ()
  deriving (Show, Eq)

-- | A Type in the program
data Type α θ ι σ where
  -- | A regular type with a name
  TypeName
    :: forall ann v int sym
     . ann
    -- ^ The annotation
    -> v
    -- ^ The name/variable
    -> Type ann v int sym
  -- |  Type application
  TypeApp
    :: forall ann v int sym
     . ann
    -- ^ The annotation
    -> Type ann v int sym
    -- ^ The LHS
    -> Type ann v int sym
    -- ^ The RHS
    -> Type ann v int sym
  -- | Built-in list type, i.e. @[a]@. Technically also a type
  -- application, but might be useful to just bake it in for pretty printing,
  -- code generation, etc.
  TypeList
    :: forall ann v int sym
     . ann
    -- ^ The annotation
    -> Type ann v int sym
    -- ^ The type of the elements
    -> Type ann v int sym
  -- | Built-in tuple type, i.e. @(a, b)@. Technically also a type
  -- application, but might be useful just to bake it in for pretty printing,
  -- code generation, etc.
  TypeTuple
    :: forall ann v int sym
     . ann
    -- ^ The annotation
    -> Type ann v int sym
    -- ^ The type of the first projection of the tuple
    -> NonEmpty (Type ann v int sym)
    -- ^ The types of the elements
    -> Type ann v int sym
  -- | The unit type ()
  TypeUnit
    :: forall ann v int sym
     . ann
    -- ^ The annotation
    -> Type ann v int sym
    -- ^ The unit type ()
    -- | The literal natural type, i.e. numbers like @0@, @1@, @42@.
  TypeNatLit
    :: forall ann v int sym
     . ann
    -- ^ The annotation
    -> int
    -- ^ The natural literal
    -> Type ann v int sym
    -- ^ The natural literal type
    -- | The literal symbol type, i.e. strings like @"foo"@.
  TypeSymbolLit
    :: forall ann v int sym
     . ann
    -- ^ The annotation
    -> sym
    -- ^ The symbol literal
    -> Type ann v int sym
    -- ^ The symbol literal type
  deriving (Show, Eq)

-- | A relation in the program.
data Relation α β π where
  Relation
    :: forall ann rel_name param
     . ann
    -- ^ The annotation
    -> rel_name
    -- ^ The name of the relation
    -> param
    -- ^ The arguments to the relation
    -> Relation ann rel_name param
  deriving (Show, Eq)

-- | A rule in the program.
data Rule α ν β π χ δ where
  Rule
    :: forall ann rule_name rule_bound_vars asm cond concl
     . ann
    -- ^ The annotation
    -> rule_name
    -- ^ The name of the rule
    -> rule_bound_vars
    -- ^ The bound variables of the rule
    -> asm
    -- ^ The assumptions (relations) of the rule
    -> cond
    -- ^ The conditions of the rule
    -> concl
    -- ^ The conclusion of the rule
    -> Rule ann rule_name rule_bound_vars asm cond concl
  deriving (Show, Eq)

-- | Fact-like premise/conclusion, i.e., an instance of a relation
data Fact α ρ β where
  Fact
    :: forall ann rel args
     . ann
    -- ^ The annotation
    -> rel
    -- ^ The relation symbol
    -> args
    -- ^ The arguments to the relation symbol
    -> Fact ann rel args
  deriving (Show, Eq)

-- | A condition of a rule.
data Condition α ε where
  Condition
    :: forall ann expr
     . ann
    -- ^ The annotation
    -> expr
    -- ^ The condition itself
    -> Condition ann expr
  deriving (Show, Eq)

-- | A Haskell source block in the program
data HsBlock α ν β where
  HsBlock
    :: forall ann name body
     . ann
    -- ^ The annotation
    -> name
    -- ^ The name
    -> body
    -- ^ The body
    -> HsBlock ann name body
  deriving (Show, Eq)

-- | A Haskell import statement in the program
data HsImport α σ where
  HsImport
    :: forall ann imp
     . ann
    -- ^ The annotation
    -> imp
    -- ^ The import
    -> HsImport ann imp
  deriving (Show, Eq)

-- | An @include@ statement for importing Fixen modules
data Include α π where
  Include
    :: forall ann path
     . ann
    -- ^ The annotation
    -> path
    -- ^ The path to the file to import
    -> Include ann path
  deriving (Show, Eq)

-- | A priority declaration
data Priority α ε ω where
  Priority
    :: forall ann expr order
     . ann
    -- ^ The annotation
    -> expr
    -- ^ The premise
    -> order
    -- ^ The conclusion of the priority assuming the premise is true
    -> Priority ann expr order
  deriving (Show, Eq)

-- | The conclusion of a priority declaration
data PriorityConclusion α ℓ ρ where
  PriorityConclusion
    :: forall ann lhs rhs
     . ann
    -- ^ The annotation
    -> lhs
    -- ^ The LHS of the priority declaration
    -> rhs
    -- ^ The RHS of the priority declaration
    -> PriorityConclusion ann lhs rhs
  deriving (Show, Eq)

-- | The instantiation of a rule, used for priority declarations.
data RuleInstantiation α ρ μ where
  RuleInstantiation
    :: forall ann rul mp
     . ann
    -- ^ The annotation
    -> rul
    -- ^ The rule being instantiated
    -> mp
    -- ^ The map/closure describing the instantiation
    -> RuleInstantiation ann rul mp
  deriving (Show, Eq)

-- | A query declaration
data Query α ρ ν μ where
  Query
    :: forall ann rel name mode
     . ann
    -- ^ The annotation
    -> rel
    -- ^ The name of the relation
    -> name
    -- ^ The name of the query
    -> mode
    -- ^ The modes of each argument of the relation
    -> Query ann rel name mode
  deriving (Show, Eq)

-- | Query modes, used to determine if they are outputs or inputs to the query.
data Mode = Plus | Minus deriving (Show, Eq)

-- | A module declaration clause for determining the Haskell module being generated.
data ModuleDeclaration α ν where
  ModuleDeclaration
    :: forall ann mod_name
     . ann
    -- ^ The annotation
    -> mod_name
    -- ^ The name of the module being generated
    -> ModuleDeclaration ann mod_name
  deriving (Show, Eq)

-- | An extern declaration, used by the symbol solver to determine which symbols
-- are defined in Haskell.
data Extern α δ where
  Extern
    :: forall ann e
     . ann
    -- ^ The annotation
    -> e
    -- ^ The declarations
    -> Extern ann e
  deriving (Show, Eq)

-- | A partial order declaration. Used to define the instance of PartialOrd for
-- specific types, and to inform Fixen how to generate an optimized(-ish)
-- database representation.
data PartialOrdDeclaration α ν τ ℓ μ where
  PartialOrdDeclaration
    :: forall ann name ty leq mlbs
     . ann
    -- ^ The annotation
    -> name
    -- ^ The name of the thing we are defining
    -> ty
    -- ^ The base type
    -> leq
    -- ^ The function that defines how elements of this type are compared
    -> mlbs
    -- ^ The function that defines how to obtain the maximal lower bounds of two elements of this type
    -> PartialOrdDeclaration ann name ty leq mlbs
  deriving (Show, Eq)

-- | Phase declaration for multi-phase FPOP, e.g., for the reduced product + widening.
data Phases α φ where
  Phases
    :: forall ann phases
     . ann
    -- ^ The annotation
    -> phases
    -- ^ The actual phase declarations
    -> Phases ann phases
  deriving (Show, Eq)

-- | A set of rule names, i.e., the contents of phase declarations.
data Ruleset α ρ where
  Ruleset
    :: forall ann rules
     . ann
    -- ^ The annotation
    -> rules
    -- ^ The actual set of rules
    -> Ruleset ann rules
  deriving (Show, Eq)

--------------------------------------------------------------------------------
--
-- Annotations
--
--------------------------------------------------------------------------------

instance GetAnnotation ann (CoreItem name ann a rec) where
  getAnnotation (CoreItem ann _) = ann

instance GetAnnotation ann (CoreDouble name ann a) where
  getAnnotation (CoreDouble ann _ _) = ann

instance SetAnnotation ann (CoreItem name ann' a rec) (CoreItem name ann a rec) where
  setAnnotation ann (CoreItem _ x) = CoreItem ann x

instance SetAnnotation ann (CoreDouble name ann' e) (CoreDouble name ann e) where
  setAnnotation ann (CoreDouble _ a b) = CoreDouble ann a b

instance GetAnnotation ann (CorePair name ann a rec) where
  getAnnotation (CorePair ann _ _) = ann

instance GetAnnotation ann (SimpleIdentifier ann v) where
  getAnnotation (SimpleIdentifier ann _) = ann

instance GetAnnotation ann (Identifier ann mod fqn sim) where
  getAnnotation (IdentifierSimpleIdentifier (SimpleIdentifier ann _)) = ann
  getAnnotation (IdentifierFullyQualifiedName (FullyQualifiedName ann _ _)) = ann

instance GetAnnotation ann (Expr ann v int str) where
  getAnnotation (ExprVar ann _) = ann
  getAnnotation (ExprApp ann _ _) = ann
  getAnnotation (ExprIntLit ann _) = ann
  getAnnotation (ExprStrLit ann _) = ann
  getAnnotation (ExprTuple ann _ _) = ann
  getAnnotation (ExprList ann _) = ann
  getAnnotation (ExprUnit ann) = ann

instance SetAnnotation ann (Expr ann v int str) (Expr ann v int str) where
  setAnnotation ann (ExprVar _ v) = ExprVar ann v
  setAnnotation ann (ExprApp _ e1 e2) = ExprApp ann e1 e2
  setAnnotation ann (ExprIntLit _ n) = ExprIntLit ann n
  setAnnotation ann (ExprStrLit _ s) = ExprStrLit ann s
  setAnnotation ann (ExprTuple _ ts ts') = ExprTuple ann ts ts'
  setAnnotation ann (ExprList _ ls) = ExprList ann ls
  setAnnotation ann (ExprUnit _) = ExprUnit ann

instance GetAnnotation ann (Type ann v int str) where
  getAnnotation (TypeName ann _) = ann
  getAnnotation (TypeApp ann _ _) = ann
  getAnnotation (TypeList ann _) = ann
  getAnnotation (TypeTuple ann _ _) = ann
  getAnnotation (TypeUnit ann) = ann
  getAnnotation (TypeNatLit ann _) = ann
  getAnnotation (TypeSymbolLit ann _) = ann

instance SetAnnotation ann (Type ann v int str) (Type ann v int str) where
  setAnnotation ann (TypeName _ v) = TypeName ann v
  setAnnotation ann (TypeApp _ t1 t2) = TypeApp ann t1 t2
  setAnnotation ann (TypeList _ t) = TypeList ann t
  setAnnotation ann (TypeTuple _ t ts) = TypeTuple ann t ts
  setAnnotation ann (TypeUnit _) = TypeUnit ann
  setAnnotation ann (TypeNatLit _ n) = TypeNatLit ann n
  setAnnotation ann (TypeSymbolLit _ s) = TypeSymbolLit ann s

instance GetAnnotation ann (Extern ann e) where
  getAnnotation (Extern ann _) = ann

instance GetAnnotation ann (ModuleDeclaration ann g) where
  getAnnotation (ModuleDeclaration ann _) = ann

instance GetAnnotation ann (PartialOrdDeclaration ann b c d e) where
  getAnnotation (PartialOrdDeclaration ann _ _ _ _) = ann

instance SetAnnotation ann (PartialOrdDeclaration ann b c d e) (PartialOrdDeclaration ann b c d e) where
  setAnnotation ann (PartialOrdDeclaration _ a b c d) = PartialOrdDeclaration ann a b c d

--------------------------------------------------------------------------------
--
-- Names
--
--------------------------------------------------------------------------------

class Named α ν | α -> ν where
  nameOf :: α -> ν

instance Named (Relation α β π) β where
  nameOf (Relation _ n _) = n

instance Named (Rule α ν β π χ δ) ν where
  nameOf (Rule _ n _ _ _ _) = n

instance Named (HsBlock α ν β) ν where
  nameOf (HsBlock _ n _) = n

instance Named (HsImport α σ) σ where
  nameOf (HsImport _ i) = i

instance Named (ModuleDeclaration ann m) m where
  nameOf (ModuleDeclaration _ m) = m

instance Named (PartialOrdDeclaration ann a b c d) a where
  nameOf (PartialOrdDeclaration _ n _ _ _) = n
