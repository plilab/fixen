{-# LANGUAGE LambdaCase #-}
module Compiler.CodeGen.Generate ( generateProgram ) where
import Compiler.CodeGen.Util
import Compiler.CodeGen.ASTCombinators
import Compiler.CodeGen.Prebaked
import Control.Monad.Reader
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Maybe (mapMaybe, catMaybes, isNothing, fromMaybe)
import Data.Functor.Compose (Compose(Compose))
import Data.Foldable (Foldable (foldl'))
import qualified Language.Haskell.Exts as H (Module(Module))
import Language.Haskell.Exts
    ( Decl,
      ModuleHead(ModuleHead),
      ModuleName(ModuleName),
      ModulePragma(LanguagePragma),
      Name(Ident), Exp, Alt )
import Syntax.Common
import Syntax.Compact

data Env = Env {
  program         :: ExplicitCompactProgram,          -- the program under compilation
  typeMap         :: HashMap Identifier [TypeExpr],   -- mapping from relation names to their type signature
  currentVarIds   :: HashMap Identifier Identifier,  -- locally used names for initial identifiers
  currentQueueVar :: Identifier                       -- locally used name for the current queue state
}

initEnv :: ExplicitCompactProgram -> Env
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
  an element `mid` of `mids` represents
    an input position if `mid == Just ident`
    an output position if `mid == Nothing`
-}
generateFilterExp :: Identifier -> [AtomExpr CVar] -> [(Identifier, Identifier)] -> Exp () -> CodeGen (Exp ())
generateFilterExp relId patExprs eqs dataExp =
  let
    enumerate = all isFirst patExprs
      where isFirst (Id (First _)) = True
            isFirst _ = False
  in
  if enumerate then
    return $
      app (qvar "S" "toList") dataExp
  else do
    freshNames <- replicateM (length patExprs) gensym
    let
      patArgs = map pVar freshNames
      dbFactExps = map var freshNames
      applicativeArgs = zipWith mkArg dbFactExps patExprs
        where
          mkArg x (Id (First _)) = app (var "pure") x
          mkArg x e = infixApp (ident "mlbs") x (atomExprToExp e)
      applicativeExp =
        foldl'
          (infixApp $ sym "<*>") (app (var "pure") (con relId))
          applicativeArgs
      filterExp = apps
        (var "foldl'")
        [ lambda [pVar "rest", pApp relId patArgs] $
            infixApp (sym "++")
              (paren applicativeExp)
              (var "rest"),
          list [] ]
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
        return . Just $ 
          alt
            (pApp (factCon name) [pVar currFact]) 
            (app (qvar "Q" "unions")
                (list $ var qv : treeExps))

    generateTrees :: [ExplicitRuleTree] -> CodeGen (Exp ())
    generateTrees rts = do
      pqVar <- asks currentQueueVar
      app (qvar "Q" "unions")
        . list . (var pqVar :)
          <$> mapM (generateTree Nothing) rts

    generateTree :: Maybe (Exp ()) -> ExplicitRuleTree -> CodeGen (Exp ())
    generateTree _ (Result cs) = generateConclusion cs
    generateTree dataExp (Branch cAssump rts) = do
      currVarIds <- asks currentVarIds
      let (CAssumption assump eqs) = cAssump
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
      filterExp <- fromMaybe . paren
                    <$> generateFilterExp name args' eqs (dbProj name) 
                    <*> return dataExp
      return $
        apps (var "foldl'") [
          paren $ lambda
            [ pVar qv,
              pApp name patVars ]
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
      return $ app (qvar "Q" "singleton") .
        paren . app (var $ factCon name) .
          paren . apps (var name) $
            map (exprToExp . substituteCVar currVarIds) args

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
    mkNames =
      let go :: Int -> [Mode] -> [CVar]
          go _ []        = []
          go n (m:modes) =
            (if m then Constrained else First) ("v" ++ show n) : go (succ n) modes
      in go 0

    generateQuery :: ModalDef -> CodeGen [Decl ()]
    generateQuery (ModalDef relId name modes) = do
      signature <- asks $ (M.! relId) . typeMap
      let
        modes' = not <$> modes
        inTypes = filterBy modes' signature
        cvars = mkNames modes'
        bNames = mapMaybe (fmap pVar . getConstrained) cvars
      filterExp <- generateFilterExp relId (fmap Id cvars) [] (dbProj relId)
      return [
          typeSig (ident name) $
            tyFuns (map concrete inTypes) $
              tyFun (tyCon "DataBase") $
                tyList (tyCon relId),
          singleFunBind (ident name)
            (bNames ++ [pVar "db"])
            filterExp
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
        let mkConArg ty = liftPrimitiveOf ty . var
        in singleFunBind (ident $ "mk" ++ name) (pVar <$> patVars1) $
            app
              (con $ factCon name)
              (paren $ apps (con name) (zipWith mkConArg typs patVars1))
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

generateProgram :: ExplicitCompactProgram -> IO (H.Module ())
generateProgram p = do
  let nv = initEnv p
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
          generateQuerries
        ]

      imps <- asks $ imports . program
      modName <- asks $ unModule . moduleDecl . program
      let userImports = map (importDecl . unImport) imps

      return $
        H.Module ()
          (Just $ ModuleHead () (ModuleName () modName) Nothing Nothing)
          [LanguagePragma () [Ident () "DeriveGeneric"]]
          (userImports ++ defaultImports)
          $ concat
            [ discreteDefs,
              subsumesDef,
              strictlySubsumesDef,
              decls,
              computeDef ]

    generateQueue = return
      [typeDecl (dHead "Queue") (tyApp (qTyCon "Q" "MaxQueue") (tyCon "Fact"))]