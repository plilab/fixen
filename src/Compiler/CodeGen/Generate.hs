{-# LANGUAGE 
  LambdaCase, 
  TupleSections, 
  NamedFieldPuns #-}
module Compiler.CodeGen.Generate ( generateProgram ) where
import Compiler.CodeGen.Util
import Compiler.CodeGen.ASTCombinators
import Compiler.CodeGen.Prebaked
import Control.Monad.Reader
import Data.Foldable (Foldable (foldl'))
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe, catMaybes, isNothing)
import qualified Language.Haskell.Exts as H (Module(Module))
import Language.Haskell.Exts
    ( Decl,
      ModuleHead(ModuleHead),
      ModuleName(ModuleName),
      ModulePragma(LanguagePragma, OptionsPragma),
      Exp, Alt, Pat, Tool (GHC) )
import Syntax.Common
import Syntax.Compact
import Common.Util (substituteAll)

data Env = Env {
  program         :: ExplicitCompactProgram,          -- the program under compilation
  typeMap         :: HashMap Identifier [TypeExpr],   -- mapping from relation names to their type signature
  currentVarIds   :: HashMap Identifier Identifier,   -- locally used names for initial identifiers
  currentQueueVar :: Identifier,                      -- locally used name for the current queue state
  indexedSigns    :: HashMap Identifier IndexedSign,  -- explicit partitioning relations' arguments into discrete and ordered ones
  debugMode       :: Bool
}

data IndexedSign
  = Indexed (NonEmpty TypeExpr) (NonEmpty TypeExpr)
  | Ordered (NonEmpty TypeExpr)
  | Discrete [TypeExpr]

initEnv :: ExplicitCompactProgram -> Bool -> Env
initEnv p@Compact { signatures } = Env p (toMap signatures) M.empty "q" (computeIndexed signatures)
  where
    toMap = M.fromList . map (\s -> (relName s, paramTypes s))
    computeIndexed = M.fromList . map (\s -> (relName s, toIndexedSign . span isDiscrete $ paramTypes s))
    isDiscrete (TVar _) = False
    isDiscrete _        = True
    toIndexedSign (ts, [])   = Discrete ts
    toIndexedSign ([], t:ts) = Ordered $ t :| ts
    toIndexedSign (t:ts, u:us) = Indexed (t :| ts) (u :| us)

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
generateFilterExp :: Identifier -> [AtomExpr CVar] -> Constraints -> Maybe (Exp ()) -> CodeGen (Exp ())
generateFilterExp relId patExprs eqSets dataExp =
  let
    enumerate = all isFirst patExprs && M.null eqSets
      where isFirst (Id (First _)) = True
            isFirst _ = False
    mkVars params = replicateM (length params) gensym
    --mkPat'    = mkPat . NE.toList
    mkPat []  = pUnit
    mkPat [x] = pVar x
    mkPat xs  = pTuple $ map pVar xs
  in
  case dataExp of
    Just e  -> return e
    Nothing ->
      if enumerate then do
        indexSign <- asks $ (M.! relId) . indexedSigns
        let
          handleUnindexed params = do
            vs <- mkVars params
            return $
              apps (qvar "S" "foldl'")
                [ lambda [pVar "rest", mkPat vs]
                    (infixApp (sym ":")
                      (apps (con relId) (var <$> vs))
                      (var "rest"))
                , list []
                , dbProj relId
                ]
        (case indexSign of
          Discrete vs   -> handleUnindexed vs
          Ordered  vs   -> handleUnindexed vs
          Indexed ks vs -> do
            keyNames <- mkVars ks
            valNames <- mkVars vs
            return $
              apps (qvar "M" "foldlWithKey'") 
              [ lambda
                  [pVar "rest", mkPat keyNames, pVar "vals"] 
                  (infixApp (sym "++")
                    (apps (qvar "S" "foldl'")
                      [ lambda
                          [pVar "acc", mkPat valNames]
                          (infixApp (sym ":")
                            (apps
                              (con relId) 
                              (map var keyNames ++ map var valNames)) 
                            (var "acc")
                          )
                      , list []
                      , var "vals"
                      ]
                    )
                    (var "rest")
                  )
              , list []
              , dbProj relId 
              ])
      else do
        -- fresh names for the lambda expression
        freshNames <- replicateM (length patExprs) gensym
        indexSign <- asks $ (M.! relId) . indexedSigns
        -- a mapping to a fresh name for each key in `eqSets`
        -- these are bound in the let block to `mlbs` of all the variables in a set
        eqSetIds <- M.fromList <$> mapM (\k -> fmap (k,) (fresh k)) (M.keys eqSets)
        let
          -- mapping from `freshNames` to the original variable name in the rule tree
          localVarIds = M.fromList . mapMaybe identify $ zip freshNames patExprs
            where identify (vid, Id cv) = Just (getCVar cv, vid)
                  identify _            = Nothing
          --patArgs = map pVar freshNames
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
          filterExp = case indexSign of
            (Indexed keys _) -> let
                (keyNames, valNames) = splitAt (length keys) freshNames
              in apps
                (qvar "M" "foldlWithKey'")
                [ lambda
                    [pVar "rest", mkPat keyNames, pVar "vals"]
                    (infixApp (sym "++")
                      (apps (var "concatMap")
                        [ lambda
                            [mkPat valNames]
                            (mkDeclsBefore applicativeExp)
                        , var "vals"
                        ]
                      )
                      (var "rest")
                    )
                , list []
                ]
            _ -> apps
              (var "foldl'")
              [ lambda [pVar "rest", mkPat freshNames]
                . mkDeclsBefore $
                    infixApp (sym "++")
                      (paren applicativeExp)
                      (var "rest"),
                list [] ]
            where
              mkDeclsBefore
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
          app filterExp (dbProj relId)

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
      let (CAssumption (Assumption name args) eqSets) = cAssump
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
        generateFilterExp name args' eqSets dataExp
      return $
        apps (var "foldl'") [
          paren $ lambda
            [ pVar qv,
              factPat ]
            rtsExp,
          qvar "Q" "empty",
          filterExp
        ]
    substituteCVar env = getCompose . substituteAll env . Compose
    substituteConstrained env = fmap $ \case
      (First v) -> First v
      (Constrained v) -> Constrained $ env M.! v

    generateConclusion :: ContinuationFact -> CodeGen (Exp ())
    generateConclusion (Proposition name args) = do
      currVarIds <- asks currentVarIds
      dbg <- asks debugMode
      let traceConclusion = if dbg
                            then app (var "traceConclusion")
                            else id
      return $ app (qvar "Q" "singleton") .
        traceConclusion .
            paren . apps (var $ contCon name) $
              map (var . getName . substituteAll currVarIds) args

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
      filterExp <- generateFilterExp relId (fmap Id modalArgs) M.empty Nothing
      return [
          typeSig (ident name) $
            tyFuns (map concrete inTypes) $
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
  conts <- asks $ continuations . program
  ords  <- asks $ priorities . program
  return
    -- Fact type declaration
    [ dataDecl "Fact" (map mkFactCase names) [derivingList ["Show", "Eq"]]
    , dataDecl "Continuation"
        ( unqualConDecl "Initial" [tyCon "Fact"] :
          -- declare continuations
          map mkContFactDecl (M.toList conts) )
        [derivingList ["Show", "Eq"]]
    -- declare continuation evaluation function
    , typeSig (ident "evaluate") (tyFun (tyCon "Continuation") (tyCon "Fact"))
    , funBind
        $ match (ident "evaluate") [pApp "Initial" [pVar "f"]] (var "f")
        : map mkEvalCase (M.toList conts)
    -- instance Ord Continuation
    , instDecl
        (iHApp (iHCon "Ord") "Continuation")
        (Just [
          insDecl . funBind
            $  (mkLeqCase conts <$> ords)
            ++ [ match (sym "<=") [pWildCard, pApp "Initial" [pWildCard]] (con "True")
               , elseLeqCase
               ]
          ]
        )
    ]
  where
    mkEvalCase (rulName, Cont ctx (Conclusion pName args)) =
      match (ident "evaluate")
        [pApp (contCon rulName) (map (varToPVar . fst) ctx)]
        . app (var $ factCon pName)
          $ apps (var pName) (exprToExp <$> args)
    mkContFactDecl (name, Cont ctx _) = unqualConDecl (contCon name) (map (concrete . snd) ctx)
    mkFactCase name = unqualConDecl (factCon name) [tyCon name]
    mkLeqCase _ (FactPriority (Rule assumps (OrdHead f1 f2))) = let
        (Assumption name1 args1) = f1
        (Assumption name2 args2) = f2
      in match (sym "<=")
        [ pApp "Initial" [pApp (factCon name1) [pApp name1 (map atomExprToPat args1)]]
        , pApp "Initial" [pApp (factCon name1) [pApp name2 (map atomExprToPat args2)]]
        ]
        (mkAssumps assumps)
    mkLeqCase conts (RulePriority (Rule assumps (OrdHead inst1 inst2))) =
      match (sym "<=")
        [ mkInstantiation conts inst1
        , mkInstantiation conts inst2 ] 
        (mkAssumps assumps)
    mkInstantiation conts (Instantiation name binds) =
      let
        cont = M.lookupDefault (error $ "no continuation for " ++ name) name conts
        vars = map fst (context cont)
      in pApp
        (contCon name)
        (map (maybe pWildCard varToPVar . (`lookup` binds)) vars)
    mkAssumps assumps = foldr1Default (infixApp (sym "&&")) (con "True") (map exprToExp assumps)
    elseLeqCase = match (sym "<=") [pWildCard, pWildCard] (con "False")

generateDataDefs :: CodeGen [Decl ()]
generateDataDefs = return [] -- we dont generate data for now

-- data type declaration, emptyDB, insertDB
generateDB :: CodeGen [Decl ()]
generateDB = do
  signs <- asks indexedSigns
  return [
      -- data DataBase declaration
      dataDecl "DataBase"
        [unqualRecDecl "DataBase" $ map mkField (M.toList signs)]
        [derivingList ["Show", "Eq"]],
      -- emptyDB definition
      typeSig (ident "emptyDB") (tyCon "DataBase"),
      patBind (pVar "emptyDB")
        (apps (con "DataBase") (map mkEmpty $ M.elems signs)),
      -- insertDB definition
      typeSig (ident "insertDB")
        (tyFuns [tyCon "Fact", tyCon "DataBase"]
          $ tyTuple [tyCon "DataBase", tyCon "Bool"]),
      singleFunBind (ident "insertDB") [pVar "fact", pVar "db"]
        . letExp [updateDef]
        . caseExp (var "fact") $
          map mkAlt (M.toList signs)
    ]
  where
    mkType []  = tyUnit
    mkType [t] = concrete t
    mkType ts  = tyTuple . map concrete $ ts
    mkType' = mkType . NE.toList
    mkValue []  = unit
    mkValue [x] = x
    mkValue xs  = tuple xs
    -- different DB representations depending on the kind of predicate
    -- the goal is to index on discrete fields
    --mkEmpty Nullary   = con "False"
    mkEmpty (Indexed _ _) = qvar "M" "empty"
    mkEmpty _             = qvar "S" "empty"
    mkField (name, Discrete ts) = mkSetDef name ts
    mkField (name, Ordered ts)  = mkSetDef name $ NE.toList ts
    mkField (name, Indexed ds ts) =
      fieldDecl
        [ident $ dbProjId name]
        (tyApps (qTyCon "M" "HashMap")
          [ mkType' ds,
            tyApp (qTyCon "S" "HashSet") (mkType' ts) ])
{-     mkField (name, Nullary) =
      fieldDecl
        [ident $ dbProjId name]
        (tyCon "Bool") -}
    mkSetDef name ts =
      fieldDecl
        [ident $ dbProjId name]
        (tyApp
          (qTyCon "S" "HashSet")
          (mkType ts))
    mkAlt (name, Ordered ts) =
      let args = idsTo $ length ts
      in mkCasePattern name args $
      -- ?? parens not needed for the proj ??
        mkUpdateOrdered name
          (var "hset")
          (app (var $ dbProjId name) (var "db"))
          (map var args)
    mkAlt (name, Indexed ds cs) =
      let discNames = idsTo $ length ds
          discKey  = mkValue $ map var discNames
          ordNames = idsFromToCount (length ds) (length cs)
          ordVars  = map var ordNames
          ordVal   = mkValue ordVars
      in mkCasePattern name (discNames ++ ordNames) $
        ifThenElse
          (apps (qvar "M" "member") [discKey, dbProj name])
          (mkUpdateOrdered name
            (apps (qvar "M" "insert") [discKey, var "hset", dbProj name])
            (apps (qsym "M" "!") [dbProj name, discKey])
            ordVars)
          (tuple
            [ recSingleUpdate (var "db")
                (unqual . ident $ dbProjId name)
                (apps
                  (qvar "M" "insert") 
                  [ discKey
                  , app (qvar "S" "singleton") ordVal
                  , dbProj name
                  ]
                )
            , con "True"
            ]
          )
    mkAlt (name, Discrete ts) =
      let varNames = idsTo $ length ts
          vars = map var varNames
          rowVal = mkValue vars
      in mkCasePattern name varNames $
        ifThenElse
          (apps (qvar "S" "member") [rowVal, dbProj name])
          (tuple [var "db", con "False"])
          (tuple
            [ recSingleUpdate (var "db")
              (unqual . ident $ dbProjId name)
              (apps (qvar "S" "insert")
                [ rowVal
                , dbProj name
                ]
              )
            , con "True"
            ]
          )
    {-
      this abstracts the case pattern:
        <rel>Fact (<rel> <args>) -> ... 
    -}
    mkCasePattern rel args =
      alt (pApp (factCon rel) [pApp rel $ map pVar args])
    {-
      this is abstracts the common aspects of updating the set of non-discrete values:
        first (\ hset -> db{<rel> = <new>}) $ 
          update <proj> <ordVars>
      where `ordVars` will either be a single value or collected into a tuple and the 
      string `rel` will be prepended with "facts" to match the definition of `DataBase`
    -}
    mkUpdateOrdered rel new proj ordVars = 
      apps (var "first") [
          lambda [pVar "hset"] $
            recSingleUpdate
              (var "db")
              (unqual . ident $ dbProjId rel)
              new,
          paren $
            apps (var "update") [
              proj,
              mkValue ordVars
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
      [typeDecl (dHead "Queue") (tyApp (qTyCon "Q" "MaxQueue") (tyCon "Continuation"))]