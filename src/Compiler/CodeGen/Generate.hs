{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Compiler.CodeGen.Generate ( generateProgram ) where
import Compiler.CodeGen.Util
import Compiler.CodeGen.ASTCombinators
import Compiler.CodeGen.Prebaked
import Control.Monad.Reader
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Maybe (mapMaybe, catMaybes, isNothing, fromMaybe)
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose(Compose))
import Data.Foldable (Foldable (foldl'))
import qualified Language.Haskell.Exts as H (Module(Module))
import Language.Haskell.Exts
    ( Decl,
      ModuleHead(ModuleHead),
      ModuleName(ModuleName),
      ModulePragma(LanguagePragma, OptionsPragma),
      Exp, Alt, Pat, Tool (GHC) )
import Syntax.Common
import Syntax.Compact

data Env = Env {
  program         :: ExplicitCompactProgram,          -- the program under compilation
  typeMap         :: HashMap Identifier [TypeExpr],   -- mapping from relation names to their type signature
  currentVarIds   :: HashMap Identifier Identifier,   -- locally used names for initial identifiers
  currentQueueVar :: Identifier,                      -- locally used name for the current queue state
  debugMode       :: Bool
}

initEnv :: ExplicitCompactProgram -> Bool -> Env
initEnv p = Env p (toMap $ signatures p) M.empty "q"
  where
    toMap = M.fromList . map (\s -> (relName s, paramTypes s))

type CodeGen = ReaderT Env IO

freshQueueVar :: CodeGen Identifier
freshQueueVar = fresh "q"

withQueueVar :: Identifier -> CodeGen a -> CodeGen a
withQueueVar v = local (\ nv -> nv { currentQueueVar = v })

withVarIds :: HashMap Identifier Identifier -> CodeGen a -> CodeGen a
withVarIds vids = local (\ nv -> nv { currentVarIds = vids })

{-
  assumes that the database variable is called "db"
  an element `p` of `patExprs` represents
    - an input position if `p == Id (Constrained v)`
    - an output position if `p == Id (First v)`
    - a literal value to be matched against if `p == Ground l`
  `eqSets` maps the original variable name in the `ImplicitRuleTree`
  to the set of distinct mutually constrained variables in the `ExplicitRuleTree`
-}
generateFilterExp :: Identifier -> [AtomExpr CVar] -> Constraints -> Exp () -> CodeGen (Exp ())
generateFilterExp relId patExprs eqSets dataExp =
  let
    enumerate = all isFirst patExprs && M.null eqSets
      where isFirst (Id (First _)) = True
            isFirst _ = False
  in
  if enumerate then
    return dataExp
  else do
    -- fresh names for the lambda expression
    freshNames <- replicateM (length patExprs) gensym
    -- a mapping to a fresh name for each key in `eqSets`
    -- these are bound in the let block to `mlbs` of all the variables in a set
    eqSetIds <- M.fromList <$> mapM (\k -> fmap (k,) (fresh k)) (M.keys eqSets)
    let
      -- mapping from `freshNames` to the original variable name in the rule tree
      localVarIds = M.fromList . mapMaybe identify $ zip freshNames patExprs
        where identify (vid, Id cv) = Just (getCVar cv, vid)
              identify _            = Nothing
      patArgs = map pVar freshNames
      dbFactExps = map var freshNames
      applicativeArgs = zipWith mkArg dbFactExps patExprs
        where
          mkArg _ (Id v)
            | let name = getCVar v, 
              M.member name eqSetIds = var (eqSetIds M.! name)
          mkArg x (Id (First _)) = app (var "pure") x
          mkArg x e              = apps (var "mlbs") [x, atomExprToExp e]
      applicativeExp =
        foldl'
          (infixApp $ sym "<*>") (app (var "pure") (con relId))
          applicativeArgs
      filterExp = apps
        (var "foldl'")
        [ lambda [pVar "rest", pApp relId patArgs]
          . mkDecls $
              infixApp (sym "++")
                (paren applicativeExp)
                (var "rest"),
          list [] ]
        where
          mkDecls
          -- if there are no `eqSets` this is a no-op
            | M.null eqSets = id
          -- otherwise a let block binding `mlbs` of all variables in 
          -- each `eqSet` to their respective fresh variable
            | otherwise  = letExp $
              M.toList eqSets <&> 
                \(k, eqSet) ->
                  let ids = map (localVarIds M.!) (S.toList eqSet)
                  in valBind (ident $ eqSetIds M.! k) $
                    if null ids then
                      error "constraint set must be nonempty"
                    else
                      foldl'
                        (\rest eid -> infixApp (sym ">>=") rest (app (var "mlbs") (var eid)))
                        (list [var $ head ids])
                      (tail ids)
    return $
      app filterExp dataExp

generateRuleForest :: CodeGen [Decl ()]
generateRuleForest = do
  rels <- asks $ map relName . signatures . program
  qv <- freshQueueVar
  alts <- mapM (withQueueVar qv . generateAlt) rels
  let defaultAlt | any isNothing alts = Just $ alt pWildCard (var qv)
                 | otherwise          = Nothing
  return [
      typeSig
        (ident "step")
        (tyFuns
          [tyCon "DataBase", tyCon "Fact", tyCon "Queue"] $
            tyCon "Queue"),
      singleFunBind
        (ident "step")
        [pVar "db", pVar "fact", pVar qv]
        . caseExp (var "fact")
        . catMaybes $ alts ++ [defaultAlt]
    ]
  where
    generateAlt :: Identifier -> CodeGen (Maybe (Alt ()))
    generateAlt name = do
      currFact <- gensym
      let hasRoot p (Branch cAssump _) = p == (headSymbolOf . assumption $ cAssump)
          hasRoot _ _ = False
      trees <- asks $ filter (hasRoot name) . getTrees . ruleForest . program
      if null trees then
        return Nothing
      else do
        treeExps <- mapM (generateTree (Just . list $ [var currFact])) trees
        qv <- asks currentQueueVar
        factPat <- mkFactPat $ pVar currFact
        return . Just $
          alt
            (pApp (factCon name) [factPat])
            (app (qvar "Q" "unions")
                (list $ var qv : treeExps))

    generateTrees :: [ExplicitRuleTree] -> CodeGen (Exp ())
    generateTrees rts = do
      dbg <- asks debugMode
      pqVar <- asks currentQueueVar
      let tracePremise =
            if dbg 
            then app (app (var "trace") (paren $ infixApp (sym "++") (stringLit "got: ") (app (var "show") (var "f")))) . paren 
            else id
      tracePremise
        . app (qvar "Q" "unions")
          . list . (var pqVar :)
            <$> mapM (generateTree Nothing) rts

    generateTree :: Maybe (Exp ()) -> ExplicitRuleTree -> CodeGen (Exp ())
    generateTree _ (Result cs) = generateConclusion cs
    generateTree dataExp (Branch cAssump rts) = do
      currVarIds <- asks currentVarIds
      let (CAssumption assump eqSets) = cAssump
          name = headSymbolOf assump
          args = argumentsOf assump
          varNames = mapMaybe getAtomName args
      newVarIds <- foldM (\rest vName ->
        if M.member vName currVarIds then do
          freshName <- fresh (currVarIds M.! vName)
          return $ M.insert vName freshName rest
        else
          return $ M.insert vName vName rest
        ) currVarIds varNames
          -- we use the current identifiers for the filter expression
          -- since we are filtering with respect to currently bound variables
      let args' = substituteConstrained currVarIds <$> args
          -- in the pattern we use the new identifiers which used in the body
          patVars = mapMaybe (
              fmap (pVar . getCVar)
              . getId
              . substituteCVar newVarIds
            ) args
      qv <- freshQueueVar
      rtsExp  <- (withVarIds newVarIds . withQueueVar qv . generateTrees) rts
      factPat <- mkFactPat (pApp name patVars)
      filterExp <- paren <$>
        generateFilterExp name args' eqSets (
          fromMaybe (dbProj name) dataExp
        )
      return $
        apps (var "foldl'") [
          paren $ lambda
            [ pVar qv,
              factPat ]
            rtsExp,
          qvar "Q" "empty",
          filterExp
        ]

    substituteCVar env = fmap (liftCVar (env M.!))
    substituteConstrained env = fmap $ \case
      (First v) -> First v
      (Constrained v) -> Constrained $ env M.! v

    generateConclusion :: Conclusion CVar -> CodeGen (Exp ())
    generateConclusion (Compose (Proposition name args)) = do
      currVarIds <- asks currentVarIds
      dbg <- asks debugMode
      let traceConclusion = if dbg
                            then app (var "traceConclusion")
                            else id
      return $ app (qvar "Q" "singleton") .
        traceConclusion .
          paren . app (var $ factCon name) .
            paren . apps (var name) $
              map (exprToExp . substituteCVar currVarIds) args

    mkFactPat :: Pat () -> CodeGen (Pat ())
    mkFactPat p = do
      dbg <- asks debugMode
      if dbg then
        return $ pAsPat (ident "f") p
      else
        return p

{-
  assumes that
    `lenght modes == length (typeMap ! relId)`
    `typeMap` maps each querried relation to its signature
-}
generateQuerries :: CodeGen [Decl ()]
generateQuerries = do
  qrys <- asks $ querries . program
  concatMapM generateQuery qrys
  where
    mkNames = mapM $ \m -> (if m then Constrained else First) <$> gensym

    generateQuery :: ModalDef -> CodeGen [Decl ()]
    generateQuery (ModalDef relId name modes) = do
      signature <- asks $ (M.! relId) . typeMap
      let modes'  = not <$> modes
          inTypes = filterBy modes' signature
      modalArgs <- mkNames modes'
      let paramNames = mapMaybe getConstrained modalArgs
          patterns = fmap pVar (paramNames ++ ["db"])
      filterExp <- generateFilterExp relId (fmap Id modalArgs) M.empty (dbProj relId)
      return [
          typeSig (ident name) $
            tyFuns (map concreteUnlifted inTypes) $
              tyFun (tyCon "DataBase") $
                tyList (tyCon relId),
          singleFunBind (ident name) patterns filterExp
        ]

generateRelations :: CodeGen [Decl ()]
generateRelations =  do
  sigs <- asks $ signatures . program
  concatMapM generateRel sigs
  where
    parensLeq x y = paren $ infixApp (ident "leq") (var x) (var y)

    generateRel :: Signature -> CodeGen [Decl ()]
    generateRel (Signature name typs) = do
      -- vars for each parameter
      let patVars1 = idsTo (length typs)
      let patVars2 = prime <$> patVars1
      return [
        -- data type declaration
        dataDecl name
          [unqualConDecl name $ map concrete typs]
          [derivingList ["Eq", "Show", "Generic"]],
        -- Hashable instance
        instDecl
          (iHApp (iHCon "Hashable") name)
          Nothing,
        -- PartialOrd instance
        instDecl
          (iHApp (iHCon "PartialOrd") name)
          $ Just [
            insDecl $ singleFunBind (ident "leq")
              [ pApp name (pVar <$> patVars1)
              , pApp name (pVar <$> patVars2) ]
              $ foldr1Default (infixApp $ sym "&&") (con "True")
                $ zipWith parensLeq patVars1 patVars2
          ],
        -- generate smart constructor
        singleFunBind (ident $ "mk" ++ name) (pVar <$> patVars1) $
          app
            (con $ factCon name)
            (paren $ apps (con name) (map var patVars1))
        ]

generateFact :: CodeGen [Decl ()]
generateFact = do
  names <- asks $ map relName . signatures . program
  return
    -- Fact type declaration
    [ dataDecl "Fact" (map mkConCase names) [derivingList ["Show", "Eq"]]
    -- instance Ord Fact
    , instDecl
        (iHApp (iHCon "Ord") "Fact")
        (Just [insDecl . funBind $ (mkLeqCase <$> names) ++ [elseLeqCase]]) ]
  where
    mkConCase name = unqualConDecl (factCon name) [tyCon name]
    mkLeqCase name =
      match (sym "<=")
        [pApp (factCon name) [pVar "x"], pApp (factCon name) [pVar "y"]]
        (apps (var "leq") [var "x", var "y"])
    elseLeqCase = match (sym "<=") [pWildCard, pWildCard] (con "False")

generateDataDefs :: CodeGen [Decl ()]
generateDataDefs = return [] -- we dont generate data for now

-- data type declaration, emptyDB, insertDB
generateDB :: CodeGen [Decl ()]
generateDB = do
  names <- asks $ map relName . signatures . program
  return [
      -- data DataBase declaration
      dataDecl "DataBase"
        [unqualRecDecl "DataBase" $ map mkField names]
        [derivingList ["Show", "Eq"]],
      -- emptyDB definition
      typeSig (ident "emptyDB") (tyCon "DataBase"),
      patBind (pVar "emptyDB")
        (apps (con "DataBase") $ replicate (length names) (qvar "S" "empty")),
      -- insertDB definition
      typeSig (ident "insertDB")
        (tyFuns [tyCon "Fact", tyCon "DataBase"]
          $ tyTuple [tyCon "DataBase", tyCon "Bool"]),
      singleFunBind (ident "insertDB") [pVar "fact", pVar "db"] $
        letExp [updateDef]
        $ caseExp (var "fact") $
          map mkAlt names
    ]
  where
    mkField name =
      fieldDecl
        [ident $ dbProjId name]
        (tyApp (qTyCon "S" "HashSet") (tyCon name))
    mkAlt name = alt (pApp (factCon name) [pVar "v"]) $
      apps (var "first") [
        lambda [pVar "hset"] $
          recSingleUpdate
            (var "db")
            (unqual . ident $ dbProjId name)
            (var "hset"),
        paren $
          apps (var "update") [
            app (var $ dbProjId name) (var "db"),
            var "v"
          ]
      ]

generateDebugDefs :: CodeGen [Decl ()]
generateDebugDefs = do
  dbg <- asks debugMode
  if dbg then
    return traceConclusionDef
  else return []

generateProgram :: Bool -> ExplicitCompactProgram -> IO (H.Module ())
generateProgram debug p = do
  let nv = initEnv p debug
  runReaderT go nv
  where
    go :: CodeGen (H.Module ())
    go = do
      decls <- concatSequence [
          generateDataDefs,
          generateRelations,
          generateFact,
          generateDB,
          generateQueue,
          generateRuleForest,
          generateQuerries,
          generateDebugDefs
        ]

      imps <- asks $ imports . program
      modName <- asks $ unModule . moduleDecl . program
      let userImports = map (importDecl . unImport) imps
          debugImps = if debug then debugImports else []
          debugPragmas = if debug then [
              {- I am adding these because the documentation of `unsafePerformIO`, which is used in debug mode, advises so.
                 The same goes for the `NOINLINE` annotation in `compute` -}
              OptionsPragma () (Just GHC) "-fno-cse -fno-full-laziness",
              {- Without this pragma some sideffects were still happening out of order -}
              LanguagePragma () [ident "Strict"]
            ] else []
          mainLoop = if debug then computeDefDebug else computeDef

      return $
        H.Module ()
          (Just $ ModuleHead () (ModuleName () modName) Nothing Nothing)
          (
            [ LanguagePragma () [ident "DeriveGeneric"],
              OptionsPragma () (Just GHC) 
              "-Wno-unused-binds -Wno-unused-matches -Wno-unused-imports -Wno-missing-signatures -Wno-missing-export-lists" ]
            ++ debugPragmas
          )
          (userImports ++ debugImps ++ defaultImports)
          $ concat
            [ subsumesDef,
              strictlySubsumesDef,
              decls,
              mainLoop ]

    generateQueue = return
      [typeDecl (dHead "Queue") (tyApp (qTyCon "Q" "MaxQueue") (tyCon "Fact"))]