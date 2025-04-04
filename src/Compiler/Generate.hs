module Compiler.Generate where

import Language.Haskell.Exts
import Syntax.Common
import Syntax.Compact

ruleTreeToHsSrc :: RuleTree CVar -> Decl ()
ruleTreeToHsSrc = undefined

compactToHsSrc :: CompactProgram CVar -> Module ()
compactToHsSrc = undefined