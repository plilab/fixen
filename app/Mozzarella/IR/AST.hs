{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Mozzarella.IR.AST where

import Data.Void
import Error.Diagnose.Position (Position)
import Mozzarella.IR.Core

-------------------------------------------------------------------------------
--
-- Base AST
--
-- Now we use these building blocks to define our types!
--
-------------------------------------------------------------------------------

type TypeLetterIdentifier = CoreItem "AST.TypeLetterIdentifier" Position String Void
pattern TypeLetterIdentifier :: Position -> String -> TypeLetterIdentifier
pattern TypeLetterIdentifier a b = CoreItem a b

type TermLetterIdentifier = CoreItem "AST.TermLetterIdentifier" Position String Void
pattern TermLetterIdentifier :: Position -> String -> TermLetterIdentifier
pattern TermLetterIdentifier a b = CoreItem a b

type OpIdentifier = CoreItem "AST.OpIdentifier" Position String Void
pattern OpIdentifier :: Position -> String -> OpIdentifier
pattern OpIdentifier a b = CoreItem a b

type TermIdentifier = TermLetterIdentifier :+: OpIdentifier
pattern TermIdentifierAlpha :: TermLetterIdentifier -> TermIdentifier
pattern TermIdentifierAlpha t <- ((↓?) -> Just t)
  where
    TermIdentifierAlpha t = (↑) t
pattern TermIdentifierOp :: OpIdentifier -> TermIdentifier
pattern TermIdentifierOp t <- ((↓?) -> Just t)
  where
    TermIdentifierOp t = (↑) t
{-# COMPLETE TermIdentifierAlpha, TermIdentifierOp #-}

type TypeIdentifier = TypeLetterIdentifier
pattern TypeIdentifier :: Position -> String -> TypeIdentifier
pattern TypeIdentifier a b = CoreItem a b

type IntLit = CoreItem "AST.IntLit" Position Integer
pattern IntLit :: Position -> Integer -> IntLit a
pattern IntLit a b = CoreItem a b

type StrLit = CoreItem "AST.StrLit" Position String
pattern StrLit :: Position -> String -> StrLit a
pattern StrLit a b = CoreItem a b

type TermVar = CoreItem "AST.TermVar" Position TermIdentifier
pattern TermVar :: Position -> TermIdentifier -> TermVar a
pattern TermVar a b = CoreItem a b

type AppExpr = CoreDouble "AST.AppExpr" Position
pattern AppExpr :: Position -> Expr -> Expr -> AppExpr Expr
pattern AppExpr pos e e' = CoreDouble pos e e'

type Expr = Fixpoint (IntLit ::+:: StrLit ::+:: TermVar ::+:: AppExpr)
pattern ExprIntLit :: IntLit Expr -> Expr
pattern ExprIntLit e <- ((↓↓?) -> Just e)
  where
    ExprIntLit e = (↑↑) e
pattern ExprStrLit :: StrLit Expr -> Expr
pattern ExprStrLit e <- ((↓↓?) -> Just e)
  where
    ExprStrLit e = (↑↑) e
pattern ExprTermVar :: TermVar Expr -> Expr
pattern ExprTermVar e <- ((↓↓?) -> Just e)
  where
    ExprTermVar e = (↑↑) e
pattern ExprApp :: AppExpr Expr -> Expr
pattern ExprApp e <- ((↓↓?) -> Just e)
  where
    ExprApp e = (↑↑) e
{-# COMPLETE ExprIntLit, ExprStrLit, ExprTermVar, ExprApp #-}

type VarType = CoreItem "AST.VarType" Position TypeIdentifier

type AppType = CoreDouble "AST.AppType" Position

type Type = Fixpoint (VarType ::+:: AppType)

mkVarType :: Position -> TypeIdentifier -> Type
mkVarType ann t = (↑↑) $ CoreItem @"AST.VarType" ann t

mkAppType :: Position -> Type -> Type -> Type
mkAppType ann t t' = (↑↑) $ CoreDouble @"AST.AppType" ann t t'

mkAppExpr :: Position -> Expr -> Expr -> Expr
mkAppExpr ann e e' = (↑↑) $ CoreDouble @"AST.AppExpr" ann e e'

type AssumptionArguments =
  CoreItem
    "AST.Assumption#arguments"
    Position
    [TermLetterIdentifier]
    Void

mkAssumptionArguments :: Position -> [TermLetterIdentifier] -> AssumptionArguments
mkAssumptionArguments = CoreItem @"AST.Assumption#arguments"

type Assumption = CorePair "AST.Assumption" Position TypeLetterIdentifier AssumptionArguments

mkAssumption :: Position -> TypeLetterIdentifier -> AssumptionArguments -> Assumption
mkAssumption = CorePair @"AST.Assumption"

type ConclusionArguments = CoreItem "AST.Conclusion#arguments" Position [Expr] Void

mkConclusionArguments :: Position -> [Expr] -> ConclusionArguments
mkConclusionArguments = CoreItem @"AST.Conclusion#arguments"

type Conclusion = CorePair "AST.Conclusion" Position TypeLetterIdentifier ConclusionArguments

mkConclusion :: Position -> TypeLetterIdentifier -> ConclusionArguments -> Conclusion
mkConclusion = CorePair @"AST.Conclusion"

type Condition = CoreItem "AST.Condition" Position Expr Void

mkCondition :: Position -> Expr -> Condition
mkCondition = CoreItem @"AST.Condition"

type Premise = Assumption :+: Condition

type RelationSignature = CoreItem "AST.Relation#signature" Position [Type] Void

mkRelationSignature :: Position -> [Type] -> RelationSignature
mkRelationSignature = CoreItem @"AST.Relation#signature"

type Completion = CoreItem "AST.Relation#completion" Position TermLetterIdentifier Void

mkCompletion :: Position -> TermLetterIdentifier -> Completion
mkCompletion = CoreItem @"AST.Relation#completion"

type Relation =
  CoreRelation
    "AST.Relation"
    Position
    TypeLetterIdentifier
    RelationSignature
    (Maybe Completion)

mkRelation :: Position -> TypeLetterIdentifier -> RelationSignature -> Maybe Completion -> Relation
mkRelation = CoreRelation @"AST.Relation"

type RulePremises = CoreItem "AST.Rule#premises" Position [Premise] Void

mkRulePremises :: Position -> [Premise] -> RulePremises
mkRulePremises = CoreItem @"AST.Rule#premises"

type Rule = CoreRule "AST.Rule" Position (Maybe TermLetterIdentifier) RulePremises Conclusion

mkRule :: Position -> Maybe TermLetterIdentifier -> RulePremises -> Conclusion -> Rule
mkRule = CoreRule @"AST.Rule"

type TopLevel = Relation :+: Rule

newtype Program = Program {topLevels :: [TopLevel]}
  deriving (Show, Eq)

getPosition :: forall group e. (group :>: Position, GetAnnotation group e) => e -> Position
getPosition = getAnnotationOf @Position

setPosition :: forall group e e'. (group :>: Position, SetAnnotation group e e', GetAnnotation group e) => Position -> e -> e'
setPosition = setAnnotationOf @Position
