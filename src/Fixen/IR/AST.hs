{-# LANGUAGE ExplicitNamespaces #-}

-- |
--     Module      : Fixen.IR.AST
--     Description : Concrete abstract syntax tree types — the parser's output
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
module Fixen.IR.AST (
  module Fixen.IR.AST,
  module D,
) where

import Data.List.NonEmpty
import Data.Map.Strict
import Data.Text (Text)
import Fixen.IR.Core as D (
  SimpleIdentifier(..),
  Expr(..),
  Type(..),
  ModuleName(..),
  FullyQualifiedName(..),
  Identifier(..),
  data Condition,
  data Extern,
  data HsBlock,
  data HsImport,
  data Include,
  data ModuleDeclaration,
  data PartialOrdDeclaration,
  data Phases,
  data Priority,
  data PriorityConclusion,
  data Program,
  data Query,
  QueryMode(..),
  data Relation,
  data Rule,
  data RuleInstance,
  data Ruleset,
  moduleName
  , hsImports
  , hsBlocks 
  , includes
  , extern 
  , relations
  , partialOrdDeclarations
  , rules 
  , priorities
  , queries
  , phases
 )
import Fixen.IR.Core qualified as Core 
import Prettyprinter
import Prettyprinter.Render.Terminal
import Fixen.Data.NodeId(NodeId, HasNodeId(..))

-- | A relation declaration as produced by the parser.
--
--   This is 'Core.Relation' instantiated with 'Core.SimpleIdentifier'
--   for the relation name and @[Core.Type]@ for the list of argument
--   types.
--
--   Example: @relation Dist : Integer, Integer@
type Relation =
  Core.Relation
    Core.SimpleIdentifier
    -- ^ The relation name (e.g. @Dist@, @MyFact@).
    [Core.Type]
    -- ^ The types of the relation's arguments.

-- | An assumption within a rule body.
--
--   This is a 'Core.Relation' applied to a list of 'Core.SimpleIdentifier'
--   variables. Assumptions appear on the left side of the turnstile (|-)
--   in a rule and assert that a relation holds for given variable arguments.
--
--   Example: @MyFact x y@ where @x@ and @y@ are variables.
type Assumption = Core.Relation Core.SimpleIdentifier [Core.SimpleIdentifier]

-- | A condition expression within a rule body.
--
--   This is 'Core.Condition' instantiated with 'Core.Expr' for the
--   condition expression. Conditions appear after the 'if' keyword
--   and must evaluate to true for the rule to fire.
--
--   Example: @if x <= y@
type Condition = Core.Condition Core.Expr

-- | A conclusion of a rule.
--
--   This is 'Core.Relation' instantiated with 'Core.SimpleIdentifier'
--   for the relation name and @[Core.Expr]@ for the argument expressions.
--   The conclusion appears on the right side of the turnstile (|-)
--   and specifies what the rule derives.
--
--   Example: @|- Dist x y@
type Conclusion = Core.Relation Core.SimpleIdentifier [Core.Expr]

-- | A rule declaration as produced by the parser.
--
--   This is 'Core.Rule' instantiated with concrete types for all
--   its parameters:
--
--   * @ν@ — 'Maybe Core.SimpleIdentifier': an optional rule name
--   * @β@ — @[Core.SimpleIdentifier]@: the bound variables
--   * @π@ — @[Assumption]@: the assumptions (relations applied to variables)
--   * @χ@ — @[Condition]@: the guard conditions
--   * @δ@ — 'Conclusion': the conclusion (a relation applied to expressions)
--
--   Example:
--
--   @
--   rule myRule x y:
--       MyFact x,
--       if x < y,
--       |- Dist x y
--   @
type Rule =
  Core.Rule
    (Maybe Core.SimpleIdentifier)
    -- ^ An optional name for the rule (e.g. @myRule@, or 'Nothing' for
    --   unnamed rules).
    [Core.SimpleIdentifier]
    -- ^ The bound variables of the rule (e.g. @[x, y]@).
    [Assumption]
    -- ^ The assumptions — relations applied to variable names that
    --   must hold for the rule to fire.
    [Condition]
    -- ^ The conditions — guard expressions (after @if@) that must
    --   evaluate to true for the rule to fire.
    Conclusion
    -- ^ The conclusion — the relation and expressions that the rule
    --   derives when it fires.

-- | An extern declaration as produced by the parser.
--
--   This is 'Core.Extern' instantiated with a 'NonEmpty' list of
--   'Core.SimpleIdentifier' values, listing symbols defined in
--   Haskell source code.
type Extern = Core.Extern (NonEmpty Core.SimpleIdentifier)

-- | A Haskell module import statement as produced by the parser.
--
--   This is 'Core.HsImport' instantiated with 'Core.ModuleName' for
--   the imported module.
--
--   Example: @import Data.List@
type HsImport = Core.HsImport Core.ModuleName

-- | An include statement as produced by the parser.
--
--   This is 'Core.Include' instantiated with 'Text' for the file path.
--   Include statements import another Fixen file.
--
--   Example: @include "Path/To/Fixen.fix"@
type Include = Core.Include Text

-- | An embedded Haskell source code block as produced by the parser.
--
--   This is 'Core.HsBlock' instantiated with 'Text' for the code
--   contents. Haskell blocks are delimited by triple backticks with
--   the @hs@ annotation in the source.
type HsBlock = Core.HsBlock Text

-- | A rule instance as produced by the parser.
--
--   This is 'Core.RuleInstance' instantiated with 'Core.SimpleIdentifier'
--   for the rule reference and a 'Map' from 'Core.SimpleIdentifier' to
--   'Core.SimpleIdentifier' for the variable substitution map.
--
--   Example: @addDist { a = a, b = b' }@
type RuleInstance = Core.RuleInstance Core.SimpleIdentifier (Map Core.SimpleIdentifier Core.SimpleIdentifier)

-- | The conclusion of a priority declaration.
--
--   This is 'Core.PriorityConclusion' instantiated with 'RuleInstance'
--   for both the left-hand side and right-hand side rule instances.
--   It compares two rule instances with an ordering symbol.
--
--   Example: @addDist { a = a } <= addDist { a = a' }@
type PriorityConclusion = Core.PriorityConclusion RuleInstance RuleInstance

-- | A priority declaration as produced by the parser.
--
--   This is 'Core.Priority' instantiated with 'Core.Expr' for the
--   premise expression and 'PriorityConclusion' for the conclusion.
--   Priorities determine rule precedence when multiple rules could fire.
--
--   Example:
--
--   @
--   priority:
--       a <= b |- addDist { a = a, b = b' } <= addDist { a = a', b = b' }
--   @
type Priority = Core.Priority Core.Expr PriorityConclusion

-- | A query declaration as produced by the parser.
--
--   This is 'Core.Query' instantiated with:
--
--   * 'Core.SimpleIdentifier' for the relation name
--   * 'Core.SimpleIdentifier' for the query name
--   * 'NonEmpty Core.QueryMode' for the input/output modes
--
--   Example: @query DistTo as distTo - +@
type Query = Core.Query Core.SimpleIdentifier Core.SimpleIdentifier (NonEmpty Core.QueryMode)

-- | A module declaration as produced by the parser.
--
--   This is 'Core.ModuleDeclaration' instantiated with 'Core.ModuleName'
--   for the generated Haskell module name.
--
--   Example: @module My.Haskell.Module where@
type ModuleDeclaration = Core.ModuleDeclaration Core.ModuleName

-- | A partial order declaration as produced by the parser.
--
--   This is 'Core.PartialOrdDeclaration' instantiated with:
--
--   * 'Core.SimpleIdentifier' for the type name
--   * 'Core.Type' for the base type
--   * 'Core.Identifier' for the less-than-or-equal function
--   * 'Core.Identifier' for the maximal lower bounds function
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
type PartialOrdDeclaration = Core.PartialOrdDeclaration Core.SimpleIdentifier Core.Type Core.Identifier Core.Identifier

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
newtype EverythingElseRuleset = EverythingElseRuleset NodeId
  -- ^ The 'NodeId' for source position tracking.
  deriving (Show, Eq)

instance HasNodeId EverythingElseRuleset where
  -- | Extract the 'NodeId' from an 'EverythingElseRuleset'.
  getNodeId (EverythingElseRuleset i) = i

-- | An explicit ruleset as produced by the parser.
--
--   This is 'Core.Ruleset' instantiated with a 'NonEmpty' list of
--   'Core.SimpleIdentifier' values — the named rules in the ruleset.
--   A ruleset represents the contents of a phase declaration.
--
--   Example: @rule1, rule2, rule3@ within a phase.
type ExplicitRuleset = Core.Ruleset (NonEmpty Core.SimpleIdentifier)

-- | Either an explicit ruleset (a named list of rules) or the
--   @*@ wildcard ('EverythingElseRuleset') representing all remaining
--   rules.
--
--   This type appears in phase declarations where each phase is either
--   a named set of rules or the wildcard @*@ (which can only appear
--   as the last phase).
type RulesetOrEverythingElse = Either ExplicitRuleset EverythingElseRuleset

-- | Phase declarations as produced by the parser.
--
--   This is 'Core.Phases' instantiated with a 'NonEmpty' list of
--   'RulesetOrEverythingElse' values. Phases organize rules into
--   execution order for multi-phase FPOP evaluation.
--
--   Example: @[{ rule1, rule2 }, { rule3 }, *]@
type Phases = Core.Phases (NonEmpty RulesetOrEverythingElse)

-- | A complete Fixen program as produced by the parser.
--
--   This is 'Core.Program' instantiated with all the concrete types
--   from this module. It represents the full AST of a Fixen program,
--   containing every construct from the module declaration through
--   queries and phases.
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
type Program =
  Core.Program
    ModuleDeclaration
    -- ^ The module declaration (e.g. @module My.Haskell.Module where@).
    [HsImport]
    -- ^ Haskell module imports.
    [HsBlock]
    -- ^ Embedded Haskell source code blocks.
    [Include]
    -- ^ Included Fixen files.
    [Extern]
    -- ^ Externally defined symbols.
    [Relation]
    -- ^ Relation declarations.
    [PartialOrdDeclaration]
    -- ^ Partial order declarations.
    [Rule]
    -- ^ Rule declarations.
    [Priority]
    -- ^ Priority declarations.
    [Query]
    -- ^ Query declarations.
    (Maybe Phases)
    -- ^ Phase declarations (optional; 'Nothing' if no phases declared).

-- | Pretty-print a list of items as a vertical list with dash prefixes.
--
--   Each item is rendered on its own line, indented by 2 spaces and
--   prefixed with a dash (\"- \"). Items are separated by line breaks.
--   Used internally by other pretty-printing functions to format
--   lists of sub-elements.
--
--   @
--   - item1
--   - item2
--   - item3
--   @
prettyList' :: [Doc ann] -> Doc ann
prettyList' items = vsep (formatItem <$> items)
  where
    -- | Format a single item: indent by 2 spaces and prefix with a dash.
    formatItem item = hang 2 (pretty "-" <+> item)

-- | Pretty-print a 'HsImport' node.
--
--   Renders the imported module name followed by its 'NodeId' in
--   parentheses.
--
--   Output format: @Data.List (42)@
prettyHsImport :: HsImport -> Doc ann
prettyHsImport Core.HsImport {Core.hsImportNodeId = p, Core.hsImportImport = module_name} =
  pretty (Core.fullIdentifier module_name) <+> (lparen <> pretty p <> rparen)

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
prettyHsBlock (Core.HsBlock p b) = lparen <> pretty p <> rparen <> line <> pretty b

-- | Pretty-print an 'Include' node.
--
--   Renders the file path followed by its 'NodeId' in parentheses.
--   Note: the field order is (path, nodeId) — the 'Include' constructor
--   stores the path first and the nodeId second.
--
--   Output format: @Path/To/Fixen.fix (17)@
prettyInclude :: Include -> Doc ann
prettyInclude (Core.Include a p) = pretty p <+> (lparen <> pretty a <> rparen)

-- | Pretty-print a 'SimpleIdentifier' with its 'NodeId' annotation.
--
--   Renders the identifier name followed by its 'NodeId' in parentheses.
--   Used by 'prettyExtern' to display extern symbols.
--
--   Output format: @myFunction (99)@
prettySimpleIdentifierWithAnnotation :: SimpleIdentifier -> Doc ann
prettySimpleIdentifierWithAnnotation (Core.SimpleIdentifier p s) = pretty s <+> (lparen <> pretty p <> rparen)

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
prettyType (Core.TypeName _ v) = annotate (color Red <> bold) $ pretty (Core.fullIdentifier v)
-- | Type application: parenthesize the LHS and RHS separated by space.
prettyType (Core.TypeApp _ lhs rhs) = lparen <> prettyType lhs <+> prettyType rhs <> rparen
-- | List type: wrap the element type in brackets.
prettyType (Core.TypeList _ t) = lbracket <> prettyType t <> rbracket
-- | Tuple type: enclose all elements (first + rest) in parentheses,
--   comma-separated.
prettyType (Core.TypeTuple _ t ls) = encloseSep lparen rparen comma (prettyType <$> (t : Data.List.NonEmpty.toList ls))
-- | Unit type: render as empty parentheses.
prettyType (Core.TypeUnit _) = lparen <> rparen
-- | Natural number literal type: render in red.
prettyType (Core.TypeNatLit _ i) = annotate (color Red) $ pretty i
-- | Symbol (string) literal type: render in yellow.
prettyType (Core.TypeSymbolLit _ s) = annotate (color Yellow) $ pretty (show s)

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
prettyRelation (Core.Relation ann name args) =
  annotate bold (pretty "relation")
    <+> annotate (color Red <> bold) (pretty (Core.fullIdentifier name))
    <+> (lparen <> pretty ann <> rparen)
    <> line
    -- | Indent the \"types:\" label and the list of argument types.
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
prettyPartialOrd (Core.PartialOrdDeclaration p n t l m) =
  annotate bold (pretty "partial ord")
    <+> annotate (color Red <> bold) (pretty (Core.fullIdentifier n))
    <+> (lparen <> pretty p <> rparen)
    <> line
    -- | Indent the three fields: type, leq, and mlbs.
    <> indent
      2
      ( (annotate (color Yellow) (pretty "type ") <> colon <+> prettyType t)
          <> line
          -- | Render the less-than-or-equal (⊑) function in red+bold.
          <> (annotate (color Yellow) (pretty "(⊑)  ") <> colon <+> (annotate (color Red <> bold) $ pretty (Core.fullIdentifier l)))
          <> line
          -- | Render the mlbs function name in red+bold.
          <> annotate (color Yellow) (pretty "mlbs ")
          <> colon
          <+> (annotate (color Red <> bold) $ pretty (Core.fullIdentifier m))
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
prettyExtern (Core.Extern _ ls) =
            line <> annotate (color Blue <> bold) (pretty "extern") <> colon <> line <> indent 2 (prettyList' (prettySimpleIdentifierWithAnnotation <$> (Data.List.NonEmpty.toList ls)))

-- | Pretty-print an entire 'Program' with syntax highlighting.
--
--   Renders the complete Fixen program AST as a multi-line document
--   with colored, structured output. The program is rendered section by
--   section in the following order:
--
--   1. /Module declaration/ — rendered in green and bold
--   2. /Imports/ — rendered in blue and bold, with each import on its own line
--   3. /Haskell source blocks/ — rendered in blue and bold, with each block on its own line
--   4. /Includes/ — rendered in blue and bold, with each include on its own line
--   5. /Extern declarations/ — rendered in blue and bold, with each symbol on its own line
--   6. /Relation declarations/ — rendered with full formatting via 'prettyRelation'
--   7. /Partial order / lattice declarations/ — rendered with full formatting via 'prettyPartialOrd'
--
--   Sections with empty lists are omitted entirely.
--
--   Note: Rules, priorities, queries, and phases are currently commented
--   out in the output. They can be enabled by uncommenting the corresponding
--   pattern-match fields and documentation lines below.
prettyProgram :: Program -> Doc AnsiStyle
-- | Pattern-match on all fields of the 'Program' record.
--   The fields for rules, priorities, queries, and phases are
--   currently commented out so they do not appear in output.
prettyProgram
  Program
    { moduleName = module_name
    , hsImports = hs_imports
    , hsBlocks = hs_blocks
    , includes = p_includes
    , extern = p_extern
    , relations = p_relations
    , partialOrdDeclarations = partial_ords
    -- , rules = p_rules
    -- , priorities = p_priorities
    -- , queries = p_queries
    -- , phases = p_phases
    } =
    let
        -- | The module declaration line, rendered in green and bold.
        --   Example output: @module My.Haskell.Module@
        mod_doc =
              annotate
                (color Green <> bold)
                ( pretty "module"
                    <+> pretty
                      (Core.fullIdentifier (Core.nameOf module_name))
                )
        -- | The imports section. Only rendered if there are imports.
        --   Each import is rendered via 'prettyHsImport' with a dash prefix.
        imports_doc =
          if Prelude.null hs_imports
            then mempty
            else line <> annotate (color Blue <> bold) (pretty "imports") <> colon <> line <> indent 2 (prettyList' (prettyHsImport <$> hs_imports))
        -- | The Haskell source blocks section. Only rendered if there are blocks.
        --   Each block is rendered via 'prettyHsBlock' with a dash prefix.
        hs_blocks_doc =
          if Prelude.null hs_blocks
            then mempty
            else line <> annotate (color Blue <> bold) (pretty "haskell source blocks") <> colon <> line <> indent 2 (prettyList' (prettyHsBlock <$> hs_blocks))
        -- | The includes section. Only rendered if there are includes.
        --   Each include is rendered via 'prettyInclude' with a dash prefix.
        include_doc =
          if Prelude.null p_includes
            then mempty
            else line <> annotate (color Blue <> bold) (pretty "includes") <> colon <> line <> indent 2 (prettyList' (prettyInclude <$> p_includes))
        -- | The extern section. Only rendered if there are extern symbols.
        --   Each symbol is rendered via 'prettyExtern' with a dash prefix.
        extern_doc =
          if Prelude.null p_extern
            then mempty
            else line <> annotate (color Blue <> bold) (pretty "extern") <> colon <> line <> indent 2 (prettyList' (prettyExtern <$> p_extern))

        -- Note: the commented-out code below was an earlier alternative
        -- implementation for rendering extern symbols individually.
        -- It is no longer used since 'prettyExtern' handles the formatting.
        -- case p_extern of
        --   Nothing -> mempty
        --   Just e ->
        --     line <> annotate (color Blue <> bold) (pretty "extern") <> colon <> line <> indent 2 (prettyList' (prettySimpleIdentifierWithAnnotation <$> (Data.List.NonEmpty.toList (Core.externSymbols e))))
        -- | The relations section. Only rendered if there are relations.
        --   Each relation is rendered via 'prettyRelation' with full formatting.
        relations_doc =
          if Prelude.null p_relations
            then mempty
            else line <> annotate (color Blue <> bold) (pretty "relations") <> colon <> line <> indent 2 (prettyList' (prettyRelation <$> p_relations))
        -- | The partial order / lattice declarations section. Only rendered
        --   if there are partial ord declarations. Each is rendered via
        --   'prettyPartialOrd' with full formatting.
        pord_doc =
          if Prelude.null partial_ords
            then mempty
            else line <> annotate (color Blue <> bold) (pretty "lattice declarations") <> colon <> line <> indent 2 (prettyList' (prettyPartialOrd <$> partial_ords))
    -- | Concatenate all sections in order. Empty sections contribute
    --   'mempty' (nothing), so they are effectively skipped.
    in  mod_doc <> imports_doc <> hs_blocks_doc <> include_doc <> extern_doc <> relations_doc <> pord_doc

