{-# LANGUAGE FunctionalDependencies #-}

module Fixen.Fields (module Fixen.Fields, module Control.Lens) where

import Control.Lens

class HasArgs σ φ | σ -> φ where
  args :: Lens' σ φ

class HasAssumptions σ φ | σ -> φ where
  assumptions :: Lens' σ φ

class HasConclusion σ φ | σ -> φ where
  conclusion :: Lens' σ φ

class HasConditions σ φ | σ -> φ where
  conditions :: Lens' σ φ

class HasContents σ φ | σ -> φ where
  contents :: Lens' σ φ

class HasDatabase σ φ | σ -> φ where
  database :: Lens' σ φ

class HasDeclaration σ φ | σ -> φ where
  declaration :: Lens' σ φ

class HasDiagnostic σ φ | σ -> φ where
  diagnostic :: Lens' σ φ

class HasExpr σ φ | σ -> φ where
  expr :: Lens' σ φ

class HasExternInfos σ φ | σ -> φ where
  externInfos :: Lens' σ φ

class HasFact σ φ | σ -> φ where
  fact :: Lens' σ φ

class HasFileMap σ φ | σ -> φ where
  fileMap :: Lens' σ φ

class HasHsBlocks σ φ | σ -> φ where
  hsBlocks :: Lens' σ φ

class HasImports σ φ | σ -> φ where
  imports :: Lens' σ φ

class HasIncludes σ φ | σ -> φ where
  includes :: Lens' σ φ

class HasJoin σ φ | σ -> φ where
  join :: Lens' σ φ

class HasKindInfos σ φ | σ -> φ where
  kindInfos :: Lens' σ φ

class HasLatticeDeclarations σ φ | σ -> φ where
  latticeDeclarations :: Lens' σ φ

class HasLatticeInfos σ φ | σ -> φ where
  latticeInfos :: Lens' σ φ

class HasLHS σ φ | σ -> φ where
  lhs :: Lens' σ φ

class HasLeaves σ φ | σ -> φ where
  leaves :: Lens' σ φ

class HasLeq σ φ | σ -> φ where
  leq :: Lens' σ φ

class HasMap σ φ | σ -> φ where
  map :: Lens' σ φ

class HasMatchInfos σ φ | σ -> φ where
  matchInfos :: Lens' σ φ

class HasMeet σ φ | σ -> φ where
  meet :: Lens' σ φ

class HasMLBs σ φ | σ -> φ where
  mlbs :: Lens' σ φ

class HasModuleName σ φ | σ -> φ where
  moduleName :: Lens' σ φ

class HasName σ φ | σ -> φ where
  name :: Lens' σ φ

class HasNodeId σ φ | σ -> φ where
  nodeId :: Lens' σ φ

class HasPartialOrdDeclarations σ φ | σ -> φ where
  partialOrdDeclarations :: Lens' σ φ

class HasPartialOrdInfos σ φ | σ -> φ where
  partialOrdInfos :: Lens' σ φ

class HasPath σ φ | σ -> φ where
  path :: Lens' σ φ

class HasPhases σ φ | σ -> φ where
  phases :: Lens' σ φ

class HasPhaseInfos σ φ | σ -> φ where
  phaseInfos :: Lens' σ φ

class HasPremise σ φ | σ -> φ where
  premise :: Lens' σ φ

class HasPriorities σ φ | σ -> φ where
  priorities :: Lens' σ φ

class HasPriorityInfos σ φ | σ -> φ where
  priorityInfos :: Lens' σ φ

class HasQueries σ φ | σ -> φ where
  queries :: Lens' σ φ

class HasQueryInfos σ φ | σ -> φ where
  queryInfos :: Lens' σ φ

class HasRelation σ φ | σ -> φ where
  relation :: Lens' σ φ

class HasRelationDeclarations σ φ | σ -> φ where
  relationDeclarations :: Lens' σ φ

class HasRelationInfos σ φ | σ -> φ where
  relationInfos :: Lens' σ φ

class HasRHS σ φ | σ -> φ where
  rhs :: Lens' σ φ

class HasRule σ φ | σ -> φ where
  rule :: Lens' σ φ

class HasRules σ φ | σ -> φ where
  rules :: Lens' σ φ

class HasRuleInfos σ φ | σ -> φ where
  ruleInfos :: Lens' σ φ

class HasTrees σ φ | σ -> φ where
  trees :: Lens' σ φ

class HasType σ φ | σ -> φ where
  ty :: Lens' σ φ

class HasTypes σ φ | σ -> φ where
  types :: Lens' σ φ

class HasUsageInfo σ φ | σ -> φ where
  usageInfo :: Lens' σ φ

class HasVar σ φ | σ -> φ where
  var :: Lens' σ φ
