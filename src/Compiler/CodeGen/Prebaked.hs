{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Compiler.CodeGen.Prebaked where

import Language.Haskell.Exts

defaultImports :: [ImportDecl ()]
defaultImports = 
  [ ImportDecl {importAnn = (), importModule = ModuleName () "Algebra.PartialOrd", importQualified = False, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Nothing},
    ImportDecl {importAnn = (), importModule = ModuleName () "Data.Bifunctor", importQualified = False, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Just (ImportSpecList () False [IThingWith () (Ident () "Bifunctor") [VarName () (Ident () "first")]])},
    ImportDecl {importAnn = (), importModule = ModuleName () "Data.Foldable", importQualified = False, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Just (ImportSpecList () False [IThingWith () (Ident () "Foldable") [VarName () (Ident () "foldl'")]])},
    ImportDecl {importAnn = (), importModule = ModuleName () "Data.Hashable", importQualified = False, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Nothing},
    ImportDecl {importAnn = (), importModule = ModuleName () "Data.HashSet", importQualified = True, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Just $ ModuleName () "S", importSpecs = Nothing},
    ImportDecl {importAnn = (), importModule = ModuleName () "Data.PQueue.Max", importQualified = True, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Just $ ModuleName () "Q", importSpecs = Nothing},
    ImportDecl {importAnn = (), importModule = ModuleName () "GHC.Generics", importQualified = False, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Just (ImportSpecList () False [IAbs () (NoNamespace ()) (Ident () "Generic")])},
    ImportDecl {importAnn = (), importModule = ModuleName () "Numeric.Natural", importQualified = False, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}
  ]

discreteDefs :: [Decl ()]
discreteDefs = [
  DataDecl () (NewType ()) Nothing (DHApp () (DHead () (Ident () "D")) (UnkindedVar () (Ident () "a"))) [QualConDecl () Nothing Nothing (ConDecl () (Ident () "D") [TyVar () (Ident () "a")])] [Deriving () Nothing [IRule () Nothing Nothing (IHCon () (UnQual () (Ident () "Eq"))),IRule () Nothing Nothing (IHCon () (UnQual () (Ident () "Show"))),IRule () Nothing Nothing (IHCon () (UnQual () (Ident () "Generic")))]],
  InstDecl () Nothing (IRule () Nothing (Just (CxSingle () (ParenA () (TypeA () (TyApp () (TyCon () (UnQual () (Ident () "Eq"))) (TyVar () (Ident () "a"))))))) (IHApp () (IHCon () (UnQual () (Ident () "PartialOrd"))) (TyParen () (TyApp () (TyCon () (UnQual () (Ident () "D"))) (TyVar () (Ident () "a")))))) (Just [InsDecl () (PatBind () (PVar () (Ident () "leq")) (UnGuardedRhs () (Var () (UnQual () (Symbol () "==")))) Nothing)]),
  InstDecl () Nothing (IRule () Nothing (Just (CxSingle () (ParenA () (TypeA () (TyApp () (TyCon () (UnQual () (Ident () "Hashable"))) (TyVar () (Ident () "a"))))))) (IHApp () (IHCon () (UnQual () (Ident () "Hashable"))) (TyParen () (TyApp () (TyCon () (UnQual () (Ident () "D"))) (TyVar () (Ident () "a")))))) (Just [InsDecl () (FunBind () [Match () (Ident () "hash") [PParen () (PApp () (UnQual () (Ident () "D")) [PVar () (Ident () "a")])] (UnGuardedRhs () (App () (Var () (UnQual () (Ident () "hash"))) (Var () (UnQual () (Ident () "a"))))) Nothing])])
  ]

subsumesDef :: [Decl ()]
subsumesDef = [
  TypeSig () [Ident () "subsumes"] (TyForall () Nothing (Just (CxSingle () (ParenA () (TypeA () (TyApp () (TyCon () (UnQual () (Ident () "PartialOrd"))) (TyVar () (Ident () "a"))))))) (TyFun () (TyVar () (Ident () "a")) (TyFun () (TyVar () (Ident () "a")) (TyCon () (UnQual () (Ident () "Bool")))))),
  PatBind () (PVar () (Ident () "subsumes")) (UnGuardedRhs () (App () (Var () (UnQual () (Ident () "flip"))) (Var () (UnQual () (Ident () "leq"))))) Nothing
  ]

strictlySubsumesDef :: [Decl ()]
strictlySubsumesDef = [
  TypeSig () [Ident () "strictlySubsumes"] (TyForall () Nothing (Just (CxSingle () (ParenA () (TypeA () (TyApp () (TyCon () (UnQual () (Ident () "PartialOrd"))) (TyVar () (Ident () "a"))))))) (TyFun () (TyVar () (Ident () "a")) (TyFun () (TyVar () (Ident () "a")) (TyCon () (UnQual () (Ident () "Bool")))))),
  FunBind () [Match () (Ident () "strictlySubsumes") [PVar () (Ident () "y"),PVar () (Ident () "x")] (UnGuardedRhs () (InfixApp () (App () (App () (Var () (UnQual () (Ident () "leq"))) (Var () (UnQual () (Ident () "x")))) (Var () (UnQual () (Ident () "y")))) (QVarOp () (UnQual () (Symbol () "&&"))) (App () (Var () (UnQual () (Ident () "not"))) (Paren () (App () (App () (Var () (UnQual () (Ident () "leq"))) (Var () (UnQual () (Ident () "y")))) (Var () (UnQual () (Ident () "x")))))))) Nothing]
  ]

updateDef :: Decl ()
updateDef = FunBind () [Match () (Ident () "update") [PVar () (Ident () "hset"),PVar () (Ident () "vnew")] (UnGuardedRhs () (If () (InfixApp () (App () (Var () (UnQual () (Ident () "not"))) (Paren () (App () (Var () (Qual () (ModuleName () "S") (Ident () "null"))) (Var () (UnQual () (Ident () "hset")))))) (QVarOp () (UnQual () (Symbol () "&&"))) (App () (App () (Var () (UnQual () (Ident () "any"))) (RightSection () (QVarOp () (UnQual () (Ident () "subsumes"))) (Var () (UnQual () (Ident () "vnew"))))) (Var () (UnQual () (Ident () "hset"))))) (Tuple () Boxed [Var () (UnQual () (Ident () "hset")),Con () (UnQual () (Ident () "False"))]) (Let () (BDecls () [PatBind () (PVar () (Ident () "hset'")) (UnGuardedRhs () (App () (App () (Var () (Qual () (ModuleName () "S") (Ident () "filter"))) (Paren () (InfixApp () (Var () (UnQual () (Ident () "not"))) (QVarOp () (UnQual () (Symbol () "."))) (LeftSection () (Var () (UnQual () (Ident () "vnew"))) (QVarOp () (UnQual () (Ident () "strictlySubsumes"))))))) (Var () (UnQual () (Ident () "hset"))))) Nothing]) (Tuple () Boxed [App () (App () (Var () (Qual () (ModuleName () "S") (Ident () "insert"))) (Var () (UnQual () (Ident () "vnew")))) (Var () (UnQual () (Ident () "hset'"))),Con () (UnQual () (Ident () "True"))])))) Nothing]

computeDef :: [Decl ()]
computeDef = [
  TypeSig () [Ident () "compute"] (TyFun () (TyList () (TyCon () (UnQual () (Ident () "Fact")))) (TyCon () (UnQual () (Ident () "DataBase")))),
  PatBind () (PVar () (Ident () "compute")) (UnGuardedRhs () (InfixApp () (App () (Var () (UnQual () (Ident () "go"))) (Var () (UnQual () (Ident () "emptyDB")))) (QVarOp () (UnQual () (Symbol () "."))) (Var () (Qual () (ModuleName () "Q") (Ident () "fromList"))))) 
    (Just (BDecls () [TypeSig () [Ident () "go"] (TyFun () (TyCon () (UnQual () (Ident () "DataBase"))) (TyFun () (TyCon () (UnQual () (Ident () "Queue"))) (TyCon () (UnQual () (Ident () "DataBase"))))), FunBind () [Match () (Ident () "go") [PVar () (Ident () "db"),PVar () (Ident () "pq")] (GuardedRhss () [GuardedRhs () [Qualifier () (App () (Var () (Qual () (ModuleName () "Q") (Ident () "null"))) (Var () (UnQual () (Ident () "pq"))))] (Var () (UnQual () (Ident () "db"))),GuardedRhs () [Qualifier () (Var () (UnQual () (Ident () "otherwise")))] (Let () (BDecls () [PatBind () (PTuple () Boxed [PVar () (Ident () "nextFact"),PVar () (Ident () "pq'")]) (UnGuardedRhs () (App () (Var () (Qual () (ModuleName () "Q") (Ident () "deleteFindMax"))) (Var () (UnQual () (Ident () "pq"))))) Nothing,PatBind () (PTuple () Boxed [PVar () (Ident () "db'"),PVar () (Ident () "changed")]) (UnGuardedRhs () (App () (App () (Var () (UnQual () (Ident () "insertDB"))) (Var () (UnQual () (Ident () "nextFact")))) (Var () (UnQual () (Ident () "db"))))) Nothing,PatBind () (PVar () (Ident () "pq''")) (UnGuardedRhs () (If () (Var () (UnQual () (Ident () "changed"))) (App () (App () (App () (Var () (UnQual () (Ident () "step"))) (Var () (UnQual () (Ident () "db'")))) (Var () (UnQual () (Ident () "nextFact")))) (Var () (UnQual () (Ident () "pq'")))) (Var () (UnQual () (Ident () "pq'"))))) Nothing]) (App () (App () (Var () (UnQual () (Ident () "go"))) (Var () (UnQual () (Ident () "db'")))) (Var () (UnQual () (Ident () "pq''")))))]) Nothing]]))
  ]
