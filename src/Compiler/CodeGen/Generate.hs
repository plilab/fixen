module Compiler.CodeGen.Generate ( generateProgram ) where
import Compiler.CodeGen.Util
import Compiler.CodeGen.ASTCombinators
import Compiler.CodeGen.Prebaked
import Control.Monad.Reader
import qualified Language.Haskell.Exts as H (Module(Module))
import Language.Haskell.Exts
    ( Decl,
      ModuleHead(ModuleHead),
      ModuleName(ModuleName),
      ModulePragma(LanguagePragma),
      Name(Ident), Exp, Alt )
import Syntax.Common
import Syntax.Compact
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose(Compose))
import Data.Unique (hashUnique, newUnique)

data Env = Env {
  program         :: CompactProgram CVar,
  -- mapping from relation names to their type signature
  typeMap         :: Map Identifier [TypeExpr],
  currentQueueVar :: Identifier
}

initEnv :: CompactProgram CVar -> Env
initEnv p = Env p (toMap $ signatures p) "q"
  where
    toMap = M.fromList . map (\s -> (relName s, paramTypes s))

type CodeGen = ReaderT Env IO

freshQueueVar :: CodeGen Identifier
freshQueueVar = liftIO $ ("q" ++) . show . hashUnique <$> newUnique

withQueueVar :: Identifier -> CodeGen a -> CodeGen a
withQueueVar v = local (\ nv -> nv { currentQueueVar = v })

{-
  assumes that the database variable is called "db"
  an element `mid` of `mids` represents
    an input position if `mid == Just ident`
    an output position if `mid == Nothing`
-}
mkFilterExp :: Identifier -> [Maybe (Either Literal Identifier)] -> Exp ()
mkFilterExp relId mids =
  let
    -- bound names from the surrounding scope
    bNames = catMaybes mids
    {- pattern arguments: 
          Nothings become `_`, 
          identifiers `name` become `name ++ '\''`, 
          literals become pattern literals  -}
    pArgs = mids <&> 
        either litToPat id
        . maybe (Right pWildCard) (fmap $ pVar . prime)
    -- pattern names at input positions
    pNamesIn = fmap prime <$> bNames
    predicate = 
      foldr1Default (infixApp $ sym "&&") (con "True") $
        zipWith appLeq pNamesIn bNames
      where
        appLeq x y = infixApp (ident "leq") (mkExp x) (mkExp y)
        mkExp = either litToExp var
    filterExp = app
      (qvar "S" "filter")
      (lambda [pApp relId pArgs] predicate)
  in (if null bNames then id else app filterExp)
      (app
        (var $ dbProj relId)
        (var "db"))

generateRuleForest :: CodeGen [Decl ()]
generateRuleForest = do
  (RF trees) <- asks $ ruleForest . program
  qv <- freshQueueVar
  let defaultAlt = alt pWildCard (var qv)
  sequence [
      return $ typeSig 
        (ident "step") 
        (tyFuns 
          [tyCon "DataBase", tyCon "Fact", tyCon "Queue"] $ 
            tyCon "Queue"),
      singleFunBind 
        (ident "step") 
        [pVar "db", pVar "fact", pVar qv] 
        . caseExp (var "fact") 
        . (++ [defaultAlt])
        <$> mapM (withQueueVar qv . generateAlt) trees
    ]
  where
    generateAlt :: (Assumption CVar, [RuleTree CVar]) -> CodeGen (Alt ())
    generateAlt (assump, trees) = alt (mkPat assump) <$> generateTrees trees
    
    generateTrees :: [RuleTree CVar] -> CodeGen (Exp ())
    generateTrees rts = do
      pqVar <- asks currentQueueVar
      app (qvar "Q" "unions") 
        . list . (var pqVar :) 
          <$> mapM (withQueueVar pqVar . generateTree) rts

    generateTree :: RuleTree CVar -> CodeGen (Exp ())
    generateTree (RT (Result cs)) = return $ mkConclusion cs
    generateTree (RT (Branch assump rts)) = do
      let Compose (Proposition name args) = assump
      qv <- freshQueueVar
      rtsExp <- (withQueueVar qv . generateTrees) rts
      return $ 
        apps (var "foldl'") [
          paren $ lambda 
            [ pVar qv, 
              pApp name $ 
                map (maybe pWildCard pVar . getFirstId) args ]
            rtsExp, --rec call
          qvar "Q" "empty",
          paren $ mkFilterExp
            (headSymbolOf assump)
            (map toMaybeEither args)
        ]

    getFirstId = getFirst <=< getId

    toMaybeEither (Ground l) = Just $ Left l
    toMaybeEither (Id cvar) = case cvar of
      (First _) -> Nothing
      (Constrained v) -> Just $ Right v

    mkConclusion (Compose (Proposition name args)) =
      app (qvar "Q" "singleton") .
        paren . app (var $ factCon name) .
          paren . apps (var name) $
            map exprToExp args

    mkPat (Compose (Proposition name args)) = 
      pApp (factCon name) [pApp name $ map mkPatArg args]
    mkPatArg (Id v) = pVar (getCVar v)
    mkPatArg (Ground l) = litToPat l

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
      let go :: Int -> [Mode] -> [Maybe Identifier]
          go _ []        = []
          go n (m:modes) = (
              if m then Just ("v" ++ show n) else Nothing
            ) : go (succ n) modes 
      in go 0

    generateQuery :: ModalDef -> CodeGen [Decl ()]
    generateQuery (ModalDef relId name modes) = do 
      signature <- asks $ (M.! relId) . typeMap
      let
        modes' = not <$> modes
        inTypes = filterBy modes' signature
        mids = mkNames modes'
        bNames = catMaybes mids
      return [
          typeSig (ident name) $
            tyFuns (map concrete inTypes) $
              tyFun (tyCon "DataBase") $
                tyApp (qTyCon "S" "HashSet") (tyCon relId),
          singleFunBind (ident name) 
            (map pVar bNames ++ [pVar "db"])
            (mkFilterExp relId $ map (fmap Right) mids)
        ]

generateRelations :: CodeGen [Decl ()]
generateRelations =  do
  sigs <- asks $ signatures . program
  concatMapM generateRel sigs
  where
    parensLeq x y = paren $ infixApp (ident "leq") (var x) (var y)

    generateRel :: Signature -> CodeGen [Decl ()]
    generateRel (Signature name typs) = liftIO $ do
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
        [ident $ dbProj name] 
        (tyApp (qTyCon "S" "HashSet") (tyCon name))
    mkAlt name = alt (pApp (factCon name) [pVar "v"]) $
      apps (var "first") [
        lambda [pVar "hset"] $ 
          recSingleUpdate 
            (var "db") 
            (unqual . ident $ dbProj name) 
            (var "hset"),
        paren $ 
          apps (var "update") [
            app (var $ dbProj name) (var "db"), 
            var "v"
          ]
      ]

generateProgram :: CompactProgram CVar -> IO (H.Module ())
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