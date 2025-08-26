{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Mozzarella.SymbolSolver where

-- The symbol solver must perform two things:
-- 1. Make sure every var is either locally bound or extern, not neither.
-- 2. Make sure there are no name clashes:
--    1. relations vs relations
--    2. relations vs Types
--    3. rules vs rules
--    4. rules vs externs
--    5. bound vars variables vs rules
--    6. bound vars vs externs
-- 3. Warn on unused variables.
--
-- Procedure:
-- Create a symbol environment. These contain:
-- highest priority symbols (the symbols that most likely specifies the
-- programmers true intent)
-- - rule names and a reference to the rule.
-- - relation names and an arity with types and reference to the relation
-- - externs declarations
--
-- walk down:
--  relations, gathering

import Control.Monad
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Text
import Error.Diagnose.Position
import Error.Diagnose.Report
import Mozzarella.Data.AlaCarte
import Mozzarella.IR.AST qualified as AST
import Mozzarella.IR.Core
import Mozzarella.IR.ExplicitBoundVars qualified as E
import Mozzarella.Monad

type NameType = CoreItem "SymbolSolver.NameType" () Text
type ApplicationType = CoreDouble "SymbolSolver.AppType" ()
type ActualType = Fixpoint (NameType ::+:: ApplicationType)

pattern NameType :: Text -> ActualType
pattern NameType t <- ((↓↓?) -> Just (CoreItem @"SymbolSolver.NameType" () t))
  where
    NameType t = (↑↑) (CoreItem @"SymbolSolver.NameType" () t)

pattern ApplicationType :: ActualType -> ActualType -> ActualType
pattern ApplicationType t t' <- ((↓↓?) -> Just (CoreDouble @"SymbolSolver.AppType" () t t'))
  where
    ApplicationType t t' = (↑↑) (CoreDouble @"SymbolSolver.AppType" () t t')

data TypeLattice
  = Dynamic
  | ActualType ActualType
  | Top
  deriving (Show, Eq)

data BoundVarInfo
  = BoundVarInfo {boundVarUsage :: [(Int, Int)], boundVarType :: TypeLattice}
  | UnknownBoundVarInfo
  deriving (Show, Eq)

data RelationInfo
  = RelationInfo
      { relArgs :: [(ActualType, Position)]
      }
  | UnknownRelationInfo
  deriving (Show, Eq)

data RuleInfo = RuleInfo
  { ruleName :: Text
  , ruleBoundVars :: Map.Map Text BoundVarInfo
  , rulePosition :: Position
  }
  deriving (Show, Eq)

data SymbolEnv = SymbolEnv
  { relations :: Map.Map Text RelationInfo
  , rules :: Map.Map Int RuleInfo
  , externs :: Map.Map Text Position
  , typeEquations :: Map Text (TypeLattice, Reason)
  }
  deriving (Show, Eq)

data Reason = Reason AST.TermIdentifier AST.Type
  deriving (Show, Eq)

preludeTermsCons :: Set.Set Text
preludeTermsCons =
  Set.fromList
    [ "Bool"
    , "False"
    , "True"
    , "Maybe"
    , "Nothing"
    , "Just"
    , "Either"
    , "Left"
    , "Right"
    , "Ordering"
    , "LT"
    , "EQ"
    , "GT"
    , "Char"
    , "String"
    , "Eq"
    , "Ord"
    , "Enum"
    , "Int"
    , "Integer"
    , "Float"
    , "Double"
    , "Rational"
    , "Ratio"
    , "Integer"
    , "Word"
    , "Num"
    , "Real"
    , "Integral"
    , "Fractional"
    , "Floating"
    , "RealFrac"
    , "RealFloat"
    , "Semigroup"
    , "Monoid"
    , "Functor"
    , "Applicative"
    , "Monad"
    , "MonadFail"
    , "Foldable"
    , "Traversable"
    , "ShowS"
    , "Show"
    , "ReadS"
    , "Read"
    , "IO"
    , "FilePath"
    , "IOError"
    , "IOException"
    ]

preludeTerms :: Set.Set Text
preludeTerms =
  Set.fromList
    [ "not"
    , "otherwise"
    , "maybe"
    , "either"
    , "fst"
    , "snd"
    , "curry"
    , "uncurry"
    , "compare"
    , "max"
    , "min"
    , "succ"
    , "pred"
    , "toEnum"
    , "fromEnum"
    , "enumFrom"
    , "enumFromThen"
    , "enumFromTo"
    , "enumFromThenTo"
    , "minBound"
    , "maxBound"
    , "negate"
    , "abs"
    , "signum"
    , "fromInteger"
    , "toRational"
    , "quot"
    , "rem"
    , "div"
    , "mod"
    , "quotRem"
    , "divMod"
    , "toInteger"
    , "recip"
    , "fromRational"
    , "pi"
    , "exp"
    , "log"
    , "sqrt"
    , "logBase"
    , "sin"
    , "cos"
    , "tan"
    , "asin"
    , "acos"
    , "atan"
    , "sinh"
    , "cosh"
    , "tanh"
    , "asinh"
    , "acosh"
    , "atanh"
    , "properFraction"
    , "truncate"
    , "round"
    , "ceiling"
    , "floor"
    , "floatRadix"
    , "floatDigits"
    , "floatRange"
    , "decodeFloat"
    , "encodeFloat"
    , "exponent"
    , "significand"
    , "scaleFloat"
    , "isNaN"
    , "isInfinite"
    , "isDenormalized"
    , "isNegativeZero"
    , "isIEEE"
    , "atan2"
    , "subtract"
    , "even"
    , "odd"
    , "gcd"
    , "lcm"
    , "fromIntegral"
    , "realToFrac"
    , "mempty"
    , "mappend"
    , "mconcat"
    , "fmap"
    , "pure"
    , "return"
    , "fail"
    , "mapM_"
    , "sequence_"
    , "foldMap"
    , "foldl"
    , "folr"
    , "foldl'"
    , "foldr1"
    , "foldl1"
    , "elem"
    , "maximum"
    , "minimum"
    , "sum"
    , "product"
    , "traverse"
    , "sequenceA"
    , "mapM"
    , "sequence"
    , "id"
    , "const"
    , "flip"
    , "until"
    , "asTypeOf"
    , "error"
    , "errorWithoutStackTrace"
    , "undefined"
    , "seq"
    , "map"
    , "filter"
    , "head"
    , "last"
    , "tail"
    , "init"
    , "null"
    , "length"
    , "and"
    , "or"
    , "any"
    , "all"
    , "concat"
    , "concatMap"
    , "scanl"
    , "scanl1"
    , "scanr1"
    , "iterate"
    , "repeat"
    , "replicate"
    , "cycle"
    , "take"
    , "drop"
    , "takeWhile"
    , "dropWhile"
    , "span"
    , "break"
    , "splitAt"
    , "notElem"
    , "lookup"
    , "zip"
    , "zip3"
    , "zipWith"
    , "zipWith3"
    , "unzip"
    , "unzip3"
    , "lines"
    , "words"
    , "unlines"
    , "unwords"
    , "showsPrec"
    , "show"
    , "showList"
    , "shows"
    , "showChar"
    , "showString"
    , "showParen"
    , "readsPrec"
    , "readList"
    , "reads"
    , "readParen"
    , "read"
    , "lex"
    , "putChar"
    , "putStr"
    , "putStrLn"
    , "print"
    , "getChar"
    , "getLine"
    , "getContents"
    , "interact"
    , "readFile"
    , "writeFile"
    , "readIO"
    , "readLn"
    , "ioError"
    , "userError"
    ]

solveSymbols :: E.Program -> MozzarellaPass MozzarellaErrors SymbolEnv
solveSymbols pgm = do
  let env = SymbolEnv Map.empty Map.empty Map.empty Map.empty
  -- do a name check on the explicitly declared stuff by the user.
  rel_name_env <- foldM foldRelNames Map.empty $ E.relations pgm
  rul_name_env <- foldM foldRuleNames Map.empty $ E.rules pgm
  extern_name_env <- case E.externs pgm of
    Just (AST.Extern _ ls) -> foldM (foldExterns rul_name_env) Map.empty ls
    Nothing -> return Map.empty
  let completions = (\(AST.Relation _ _ _ comp) -> comp) <$> E.relations pgm
  extern_env <- foldM (foldCompletions rul_name_env) extern_name_env completions
  failIfErrored
  -- walk rels
  rel_env <- foldM (walkRelations) Map.empty $ E.relations pgm
  -- walk rules
  rul_env <- walkRules rel_env extern_env rul_name_env Map.empty $ E.rules pgm
  return env {relations = rel_env, externs = extern_env}

walkRules :: Map.Map Text RelationInfo -> Map.Map Text Position -> Map.Map Text E.Rule -> Map.Map Int RuleInfo -> [E.Rule] -> MozzarellaPass MozzarellaErrors (Map.Map Int RuleInfo)
walkRules rel_env extern_env rule_name_env mp_ ls = go mp_ 0 ls
  where
    go :: Map.Map Int RuleInfo -> Int -> [E.Rule] -> MozzarellaPass MozzarellaErrors (Map.Map Int RuleInfo)
    go mp _ [] = return mp
    go mp n (rule : rules) = do
      rule' <- walkRule rule
      go (Map.insert n rule' mp) (n + 1) rules
    walkRule :: E.Rule -> MozzarellaPass MozzarellaErrors RuleInfo
    walkRule (E.Rule rule_pos maybe_name (E.RuleBoundVars rbv_pos rule_bound_vars) premises conclusion) = do
      are_bound_vars_unique Map.empty rule_bound_vars
      are_bvs_rule_names rule_bound_vars
      undefined
    are_bound_vars_unique :: Map.Map Text E.BoundVar -> [E.BoundVar] -> MozzarellaPass MozzarellaErrors ()
    are_bound_vars_unique _ [] = return ()
    are_bound_vars_unique s (bv@(E.BoundVar bv_pos v) : bvs) = do
      case Map.lookup v s of
        Just (E.BoundVar bv_pos' v') ->
          case (bv_pos, bv_pos') of
            (E.SourcePosition pos, E.SourcePosition pos') -> do
              failR $ Err Nothing "duplicate bound variables" [(pos', This $ "bound variable called " ++ unpack v'), (pos, This $ "bound variable also called " ++ unpack v)] [Note "cannot have duplicate bound variables"]
            _ -> error "MOZZARELLA EXCEPTION: CANNOT BE THE CASE. TODO: CLEAN THIS UP!"
        Nothing ->
          are_bound_vars_unique (Map.insert v bv s) bvs
    are_bvs_rule_names :: [E.BoundVar] -> MozzarellaPass MozzarellaErrors ()
    are_bvs_rule_names [] = return ()
    are_bvs_rule_names (E.BoundVar bv_pos v : bvs) = do
      case Map.lookup v rule_name_env of
        Just (E.Rule rule_pos _ _ premises _) -> do
          let notes = case bv_pos of
                E.SourcePosition pos -> [(rule_pos, Where $ "rule is named " ++ unpack v), (pos, This $ "variable is also named " ++ unpack v)]
                E.Inferred args ->
                  let ls' = f premises <$> args
                  in  (rule_pos, Where $ "rule is named " ++ unpack v) : ls'

          failR $ Err Nothing "variable has same name as rule" notes []
        Nothing -> are_bvs_rule_names bvs

f :: AST.RulePremises -> (Int, Int) -> (Position, Marker String)
f (AST.RulePremises _ ls) (x, y) =
  let ass = ls !! x
  in  case ass of
        AST.PremiseAssumption _ _ (AST.AssumptionArguments _ args) ->
          let (CoreItem pos v) = args !! y
          in  (pos, This $ "occurrences of " ++ unpack v)
        _ -> error "MOZZARELLA EXCEPTION: CANNOT BE THE CASE. WHEN WE ANNOTATE BOUND VARIABLES WITH OCCURRENCES (I.E. FORMING OUR OWN BOUND VAR LIST), THEY MUST COME FROM ASSUMPTIONS, NOT CONDITIONS!!!!!"

walkRelations :: Map.Map Text RelationInfo -> AST.Relation -> MozzarellaPass MozzarellaErrors (Map.Map Text RelationInfo)
walkRelations mp (AST.Relation _ (AST.TypeLetterIdentifier _ n) (AST.RelationSignature _ ls) _) = do
  let t = createRawType <$> ls
  return $ Map.insert n (RelationInfo t) mp

createRawType :: AST.Type -> (ActualType, Position)
createRawType (AST.TypeVar pos (AST.TypeIdentifier _ n)) = (NameType n, pos)
createRawType (AST.TypeApp pos t t') = (ApplicationType (createRawType' t) (createRawType' t'), pos)

createRawType' :: AST.Type -> ActualType
createRawType' (AST.TypeVar _ (AST.TypeIdentifier _ n)) = NameType n
createRawType' (AST.TypeApp _ t t') = ApplicationType (createRawType' t) (createRawType' t')

foldCompletions :: Map.Map Text E.Rule -> Map.Map Text Position -> Maybe AST.Completion -> MozzarellaPass MozzarellaErrors (Map.Map Text Position)
foldCompletions mp mp2 Nothing = return mp2
foldCompletions mp mp2 (Just (AST.Completion pos (AST.TermLetterIdentifier _ n))) = do
  case Map.lookup n mp of
    Just (CoreRule pos1 _ _ _ _) -> do
      accumR $
        Err
          Nothing
          "rule and completion names clash"
          [ (pos1, Where $ "A rule named " ++ unpack n ++ " is declared here")
          , (pos, This $ "A completion named " ++ unpack n ++ " is imported here")
          , (pos1, Maybe "change the name of this rule")
          ]
          [Note "rules in the program cannot have the same name as a completion (it is externally declared)"]
      return mp2
    Nothing -> case Map.lookup n mp2 of
      Nothing -> return $ Map.insert n pos mp2
      Just _ -> return mp2

foldExterns :: Map.Map Text E.Rule -> Map.Map Text Position -> AST.TermLetterIdentifier -> MozzarellaPass MozzarellaErrors (Map.Map Text Position)
foldExterns mp mp2 (AST.TermLetterIdentifier p n) = do
  case Map.lookup n mp of
    Just (CoreRule pos1 _ _ _ _) -> do
      accumR $
        Err
          Nothing
          "rule and extern names clash"
          [ (pos1, Where $ "A rule named " ++ unpack n ++ " is declared here")
          , (p, This $ "An external declaration named " ++ unpack n ++ " is imported here")
          , (pos1, Maybe "change the name of this rule")
          ]
          [Note "rules in the program cannot have the same name as an external declaration"]
      return mp2
    Nothing -> case Map.lookup n mp2 of
      Just p2 -> do
        accumR $
          Warn Nothing "duplicate extern declarations" [(p2, Where "imported here"), (p, This "also imported here")] [Hint "extern declarations need only be imported once"]
        return mp2
      Nothing -> return $ Map.insert n p mp2

foldRelNames
  :: Map.Map Text AST.Relation
  -> AST.Relation
  -> MozzarellaPass MozzarellaErrors (Map.Map Text AST.Relation)
foldRelNames mp rel@(CoreRelation pos2 (CoreItem p n2) _ _) = do
  when
    (Set.member n2 preludeTermsCons)
    ( accumR $
        Err
          Nothing
          "relation name clashes with Haskell Prelude name"
          [(pos2, This "relation name already defined in Haskell's Prelude")]
          [Note "use unique names to avoid name clashes"]
    )
  case Map.lookup n2 mp of
    Just (CoreRelation pos1 _ _ _) -> do
      accumR $
        Err
          Nothing
          "duplicate relation names"
          [ (pos1, Where $ "A relation named " ++ unpack n2 ++ " already declared here")
          , (pos2, This $ "A relation named " ++ unpack n2 ++ " declared here")
          , (p, Maybe "change the name of this relation")
          ]
          [Note "relations in the program must have unique names"]
      return mp
    Nothing -> return $ Map.insert n2 rel mp

foldRuleNames :: Map.Map Text E.Rule -> E.Rule -> MozzarellaPass MozzarellaErrors (Map.Map Text E.Rule)
foldRuleNames mp rul@(CoreRule pos (Just (AST.TermLetterIdentifier p n)) _ _ _) = do
  when
    (Set.member n preludeTerms)
    ( accumR $
        Err
          Nothing
          "rule name clashes with Haskell Prelude name"
          [(pos, This "rule name already defined in Haskell's Prelude")]
          [Note "use unique names to avoid name clashes"]
    )
  case Map.lookup n mp of
    Just (CoreRule pos1 _ _ _ _) -> do
      accumR $
        Err
          Nothing
          "duplicate rule names"
          [ (pos1, Where $ "A rule named " ++ unpack n ++ " already declared here")
          , (pos, This $ "A rule named " ++ unpack n ++ " declared here")
          , (p, Maybe "change the name of this rule")
          ]
          [Note "rule in the program must have unique names"]
      return mp
    Nothing -> return $ Map.insert n rul mp
foldRuleNames mp _ = return mp
