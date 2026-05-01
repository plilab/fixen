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
  Named(..),
  FullyQualifiedName(..),
  Identifier(..),
  IdentifierLike(..),
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
  , partialOrdDeclarationType
  , partialOrdDeclarationLeq
  , partialOrdDeclarationMlbs
  , relationParams
  , queryRel
  , ruleName
  , ruleBoundVars
  , ruleAssumptions
  , ruleConditions
  , ruleConclusion
  , conditionExpr
  , relationName
  , phasesPhases
  , ruleSetRules
  , priorityPremise
  , priorityConclusionLHS
  , priorityConclusionRHS
  , priorityConclusion
  , ruleInstanceRule
  , ruleInstanceMap
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

-- | A relation that is queried.
--
-- This is 'Core.Relation' instantiated with:
--
-- * 'Core.SimpleIdentifier' for the relation name
-- * '[Core.QueryMode]' for the arguments
type QueriedRelation = Core.Relation Core.SimpleIdentifier [Core.QueryMode]

-- | A query declaration as produced by the parser.
--
--   This is 'Core.Query' instantiated with:
--
--   * 'QueriedRelation' for the queried relation
--   * 'Core.SimpleIdentifier' for the query name
--
--   Example: @query DistTo as distTo - +@
type Query = Core.Query QueriedRelation Core.SimpleIdentifier

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
prettyExpr (Core.ExprVar _ (Core.MkIdentifierSimple _ n)) = pretty n
prettyExpr (Core.ExprVar _ (Core.MkIdentifierFQN _ _ n)) = pretty (Core.fullIdentifier n)
prettyExpr (Core.ExprApp _ f a) = lparen <> prettyExpr f <+> prettyExpr a <> rparen
prettyExpr (Core.ExprIntLit _ i) = annotate (color Green) $ pretty i
prettyExpr (Core.ExprStrLit _ s) = annotate (color Yellow) $ pretty (show s)
prettyExpr (Core.ExprTuple _ f rs) =
  lparen <> prettyExpr f <> comma <> prettyExpr (Data.List.NonEmpty.head rs)
    <> Prelude.foldr (\r acc -> comma <+> prettyExpr r <> acc) mempty (Data.List.NonEmpty.toList rs)
    <> rparen
prettyExpr (Core.ExprList _ ls) = lbracket <> sep (punctuate comma (prettyExpr <$> ls)) <> rbracket
prettyExpr (Core.ExprUnit _) = lparen <> rparen

-- | Pretty-print an 'Assumption' (relation applied to variable names).
--
--   @
--   Dist@42 x, y
--   Edge@43 a, b, d
--   @
prettyAssumption :: Assumption -> Doc AnsiStyle
prettyAssumption (Core.Relation _ name args) =
  annotate (color Red) (pretty (Core.fullIdentifier name))
    <+> sep [pretty (Core.fullIdentifier a) | a <- args]

-- | Pretty-print a 'Conclusion' (the |- part of a rule).
--
--   @
--   |- Dist@42 x (y + 1)
--   @
prettyConclusion :: Conclusion -> Doc AnsiStyle
prettyConclusion (Core.Relation _ name args) =
  annotate bold (pretty "|-") <+> annotate (color Red) (pretty (Core.fullIdentifier name))
    <+> sep (prettyExpr <$> args)

-- | Pretty-print a 'Condition' (an @if@ guard).
--
--   @
--   if x <= y
--   @
prettyCondition :: Condition -> Doc AnsiStyle
prettyCondition (Core.Condition _ expr) =
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
prettyRule (Core.Rule _ name vars assumps conds concl) =
  let name_doc = case name of
        Nothing -> mempty
        Just n  -> pretty " " <> annotate (color Red) (pretty (Core.fullIdentifier n))
      vars_doc = pretty "boundVars:" <+> sep [pretty (Core.fullIdentifier v) | v <- vars]
      assump_doc = pretty "assumptions:" <> line <> indent 2 (vsep (prettyAssumption <$> assumps))
      cond_doc = pretty "conditions:" <> line <> indent 2 (vsep (prettyCondition <$> conds))
      concl_doc = pretty "conclusion:" <> line <> indent 2 (prettyConclusion concl)
  in  annotate bold (pretty "rule") <> name_doc
        <> line <> indent 2 vars_doc
        <> line <> indent 2 assump_doc
        <> line <> indent 2 cond_doc
        <> line <> indent 2 concl_doc

-- | Pretty-print a 'RuleInstance' (rule with variable substitution map).
--
--   @
--   addDist@7 { d = d1, d' = d1' }
--   @
prettyRuleInstance :: RuleInstance -> Doc AnsiStyle
prettyRuleInstance (Core.RuleInstance _ rule m) =
  if Data.Map.Strict.null m
    then annotate (color Red) (pretty (Core.fullIdentifier rule))
    else annotate (color Red) (pretty (Core.fullIdentifier rule))
      <> pretty " {"
      <> sep (punctuate comma [pretty (Core.fullIdentifier k) <> pretty " = " <> pretty (Core.fullIdentifier v) | (k, v) <- Data.Map.Strict.toAscList m])
      <> pretty " }"

-- | Pretty-print a 'PriorityConclusion' (lhs @op@ rhs).
--
--   @
--   addDist@7 <= addDist@8
--   @
prettyPriorityConclusion :: PriorityConclusion -> Doc AnsiStyle
prettyPriorityConclusion (Core.PriorityConclusion _ lhs rhs) =
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
prettyPriority (Core.Priority _ premise concl) =
  annotate bold (pretty "priority:")
    <> line
    <> indent 2 (prettyExpr premise <> pretty " |- " <> prettyPriorityConclusion concl)

-- | Pretty-print a 'QueryMode' (@+@ for input, @-@ for output).
prettyQueryMode :: QueryMode -> Doc AnsiStyle
prettyQueryMode Core.Input{} = annotate (color Green) (pretty "+")
prettyQueryMode Core.Output{} = annotate (color Green) (pretty "-")

-- | Pretty-print a 'Query' declaration.
--
--   @
--   query distTo: DistTo@10 - +
--   @
prettyQuery :: Query -> Doc AnsiStyle
prettyQuery (Core.Query _ rel qname) =
  annotate bold (pretty "query")
    <+> annotate (color Red) (pretty (Core.fullIdentifier qname))
    <> pretty ": "
    <> annotate (color Red) (pretty (Core.fullIdentifier $ nameOf rel))
    <+> sep (prettyQueryMode <$> (relationParams rel))

-- | Pretty-print a 'RulesetOrEverythingElse'.
--
--   @
--   { rule1@1, rule2@2 }
--   *
--   @
prettyRulesetOrEverythingElse :: RulesetOrEverythingElse -> Doc AnsiStyle
prettyRulesetOrEverythingElse (Left (Core.Ruleset _ rules)) =
  lbrace <> sep (punctuate comma [pretty (Core.fullIdentifier r) <> pretty "@" <> pretty (Core.simpleIdentifierNodeId r) | r <- Data.List.NonEmpty.toList rules]) <> rbrace
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
prettyPhases (Core.Phases _ phases) =
  annotate bold (pretty "phases:")
    <> line
    <> indent 2 (vsep (prettyRulesetOrEverythingElse <$> Data.List.NonEmpty.toList phases))

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
    , extern = p_extern
    , relations = p_relations
    , partialOrdDeclarations = partial_ords
    , rules = p_rules
    , priorities = p_priorities
    , queries = p_queries
    , phases = p_phases
    } =
    let
        mod_doc =
          annotate (color Green <> bold)
            ( pretty "module"
                <+> pretty (Core.fullIdentifier (Core.nameOf module_name))
            )

        imports_doc =
          if Prelude.null hs_imports
            then mempty
            else line <> annotate (color Blue <> bold) (pretty "imports") <> colon
              <> line <> indent 2 (prettyList' (prettyHsImport <$> hs_imports))

        hs_blocks_doc =
          if Prelude.null hs_blocks
            then mempty
            else line <> annotate (color Blue <> bold) (pretty "haskell source blocks") <> colon
              <> line <> indent 2 (prettyList' (prettyHsBlock <$> hs_blocks))

        include_doc =
          if Prelude.null p_includes
            then mempty
            else line <> annotate (color Blue <> bold) (pretty "includes") <> colon
              <> line <> indent 2 (prettyList' (prettyInclude <$> p_includes))

        extern_doc =
          if Prelude.null p_extern
            then mempty
            else line <> annotate (color Blue <> bold) (pretty "extern") <> colon
              <> line <> indent 2 (prettyList' (prettyExtern <$> p_extern))

        relations_doc =
          if Prelude.null p_relations
            then mempty
            else line <> annotate (color Blue <> bold) (pretty "relations") <> colon
              <> line <> indent 2 (prettyList' (prettyRelation <$> p_relations))

        pord_doc =
          if Prelude.null partial_ords
            then mempty
            else line <> annotate (color Blue <> bold) (pretty "lattice declarations") <> colon
              <> line <> indent 2 (prettyList' (prettyPartialOrd <$> partial_ords))

        rules_doc =
          if Prelude.null p_rules
            then mempty
            else line <> annotate (color Blue <> bold) (pretty "rules") <> colon
              <> line <> indent 2 (prettyList' (prettyRule <$> p_rules))

        priorities_doc =
          if Prelude.null p_priorities
            then mempty
            else line <> annotate (color Blue <> bold) (pretty "priorities") <> colon
              <> line <> indent 2 (prettyList' (prettyPriority <$> p_priorities))

        queries_doc =
          if Prelude.null p_queries
            then mempty
            else line <> annotate (color Blue <> bold) (pretty "queries") <> colon
              <> line <> indent 2 (prettyList' (prettyQuery <$> p_queries))

        phases_doc =
          case p_phases of
            Nothing -> mempty
            Just p  -> line <> annotate (color Blue <> bold) (pretty "phases") <> colon
              <> line <> indent 2 (prettyPhases p)
    in  mod_doc <> imports_doc <> hs_blocks_doc <> include_doc <> extern_doc
           <> relations_doc <> pord_doc <> rules_doc <> priorities_doc
           <> queries_doc <> phases_doc

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
