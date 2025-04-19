{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Compiler.CodeGen.ASTCombinators where

import Compiler.CodeGen.Util
import Data.Foldable (foldl')
import Language.Haskell.Exts
    ( ConDecl(ConDecl, RecDecl),
      DataOrNew(DataType),
      Decl(FunBind, DataDecl, TypeDecl, InstDecl, PatBind, TypeSig),
      DeclHead(DHead),
      Deriving(..),
      Exp(Paren, Var, Con, InfixApp, Tuple, Let, Case, Lambda, RecUpdate, List, Lit),
      ImportDecl(ImportDecl),
      InstDecl(InsDecl),
      InstHead(IHApp, IHCon, IHParen),
      InstRule(IRule),
      Match(Match),
      ModuleName(ModuleName),
      Name(..),
      Pat(PParen, PVar, PApp, PWildCard, PLit),
      QName(UnQual, Qual),
      QOp(QVarOp),
      QualConDecl(..),
      Rhs(UnGuardedRhs),
      Sign(Signless),
      Type(TyCon, TyApp, TyFun, TyTuple, TyList), Boxed (Boxed), Binds (BDecls), Alt (Alt), FieldUpdate (FieldUpdate), FieldDecl (FieldDecl) )
import qualified Language.Haskell.Exts as H
import Syntax.Common (TypeExpr(..), Literal (LInt, LBool, LString), Expr(..), CVar, AtomExpr(..), getCVar)

ident :: String -> Name ()
ident = Ident ()

sym :: String -> Name ()
sym = Symbol ()

qual :: String -> Name () -> QName ()
qual modname = Qual () (ModuleName () modname)

unqual :: Name () -> QName ()
unqual = UnQual ()

tyCon :: String -> Type ()
tyCon = TyCon () . unqual . ident

qTyCon :: String -> String -> Type ()
qTyCon modname = TyCon () . qual modname . ident

tyApp :: Type () -> Type () -> Type ()
tyApp = TyApp ()

tyFun :: Type () -> Type () -> Type ()
tyFun = TyFun ()

tyFuns :: [Type ()] -> Type () -> Type ()
tyFuns = flip $ foldr tyFun

tyTuple :: [Type ()] -> Type ()
tyTuple = TyTuple () Boxed

tyList :: Type () -> Type ()
tyList = TyList ()

dHead :: String -> DeclHead ()
dHead = DHead () . ident 

unqualConDecl :: String -> [Type ()] -> QualConDecl ()
unqualConDecl name = QualConDecl () Nothing Nothing . ConDecl () (ident name)

dataDecl :: String -> [QualConDecl ()] -> [Deriving ()] -> Decl ()
dataDecl name = DataDecl () (DataType ()) Nothing (dHead name)

unqualRecDecl :: String -> [FieldDecl ()] -> QualConDecl ()
unqualRecDecl name = QualConDecl () Nothing Nothing . RecDecl () (ident name)

fieldDecl :: [Name ()] -> Type () -> FieldDecl ()
fieldDecl = FieldDecl ()

derivingList :: [String] -> Deriving ()
derivingList = Deriving () Nothing . map (IRule () Nothing Nothing . IHCon () . UnQual () . Ident ())

importDecl :: String -> ImportDecl ()
importDecl path = ImportDecl () (ModuleName () path) False False False Nothing Nothing Nothing

typeDecl :: DeclHead () -> Type () -> Decl ()
typeDecl = TypeDecl ()

typeSig :: Name () -> Type () -> Decl ()
typeSig name = TypeSig () [name]

instDecl :: InstHead () -> Maybe [InstDecl ()] -> Decl ()
instDecl = InstDecl () Nothing . IRule () Nothing Nothing 

patBind :: Pat () -> Exp () -> Decl ()
patBind pat rhs = PatBind () pat (UnGuardedRhs () rhs) Nothing

funBind :: [Match ()] -> Decl ()
funBind = FunBind ()

singleFunBind :: Name () -> [Pat ()] -> Exp () -> Decl ()
singleFunBind name pats rhs = funBind [match name pats rhs]

match :: Name () -> [Pat ()] -> Exp () -> Match ()
match name pats rhs = Match () name pats (UnGuardedRhs () rhs) Nothing

iHCon :: String -> InstHead ()
iHCon = IHCon () . unqual . ident

iHParen :: InstHead () -> InstHead ()
iHParen = IHParen ()

iHApp :: InstHead () -> String -> InstHead ()
iHApp h = IHApp () h . tyCon

iRule :: InstHead () -> InstRule ()
iRule = IRule () Nothing Nothing

insDecl :: Decl () -> InstDecl ()
insDecl = InsDecl ()

pVar :: String -> Pat ()
pVar = PVar () . ident

pApp :: String -> [Pat ()] -> Pat ()
pApp name = PApp () (unqual $ ident name)

pParen :: Pat () -> Pat ()
pParen = PParen ()

pWildCard :: Pat ()
pWildCard = PWildCard ()

var :: String -> Exp ()
var = Var () . unqual . ident

qvar :: String -> String -> Exp ()
qvar modul name = Var () (qual modul $ ident name)

con :: String -> Exp ()
con = Con () . unqual . ident

infixApp :: Name () -> Exp () -> Exp () -> Exp ()
infixApp op e1 = InfixApp () e1 (QVarOp () $ unqual op)

paren :: Exp () -> Exp ()
paren = Paren ()

app :: Exp () -> Exp () -> Exp ()
app = H.App ()

apps :: Exp () -> [Exp ()] -> Exp ()
apps = foldl' app

tuple :: [Exp ()] -> Exp ()
tuple = Tuple () Boxed

list :: [Exp ()] -> Exp ()
list = List ()

letExp :: [Decl ()] -> Exp () -> Exp ()
letExp = Let () . BDecls ()

caseExp :: Exp () -> [Alt ()] -> Exp ()
caseExp = Case () 

alt :: Pat () -> Exp () -> Alt ()
alt pat rhs = Alt () pat (UnGuardedRhs () rhs) Nothing

lambda :: [Pat ()] -> Exp () -> Exp ()
lambda = Lambda ()

recUpdate :: Exp () -> [FieldUpdate ()] -> Exp ()
recUpdate = RecUpdate ()

recSingleUpdate :: Exp () -> QName () -> Exp () -> Exp ()
recSingleUpdate e qname e' = recUpdate e [fieldUpdate qname e']

fieldUpdate :: QName () -> Exp () -> FieldUpdate ()
fieldUpdate = FieldUpdate ()

dbProj :: String -> Exp ()
dbProj relId = app (var $ dbProjId relId) (var "db")

concrete :: TypeExpr -> Type ()
concrete (TVar v) = tyCon v
concrete ty       = tyApp (tyCon "D") (tyCon $ show ty)

liftPrimitiveOf :: TypeExpr -> Exp () -> Exp ()
liftPrimitiveOf (TVar _) = id
liftPrimitiveOf _        = app (con "D")

exprToExp :: Expr CVar -> Exp ()
exprToExp (App op args) = apps (var op) $ map exprToExp args
exprToExp (Atom at)     = atomExprToExp at

atomExprToExp :: AtomExpr CVar -> Exp ()
atomExprToExp (Id v)     = var $ getCVar v
atomExprToExp (Ground l) = litToExp l

litToExp :: Literal -> Exp ()
litToExp (LInt n)    = Lit () $ H.Int () (toInteger n) (show n)
litToExp (LBool b)   = con $ show b
litToExp (LString s) = Lit () $ H.String () s (show s)

litToPat :: Literal -> Pat ()
litToPat (LInt n)    = PLit () (Signless ()) $ H.Int () (toInteger n) (show n)
litToPat (LBool b)   = pApp (show b) []
litToPat (LString s) = PLit () (Signless ()) $ H.String () s (show s)

atomExprToPat :: AtomExpr CVar -> Pat ()
atomExprToPat (Id v) = pVar $ getCVar v
atomExprToPat (Ground l) = litToPat l