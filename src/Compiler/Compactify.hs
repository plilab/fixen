module Compiler.Compactify ( compactify ) where

import Common.Util
import Data.Foldable (Foldable(foldr'), find)
import Data.Maybe (mapMaybe)
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Syntax.Common
import qualified Syntax.Sorted as P
import Syntax.Compact
import Prettyprinter

data UnordRule = UnordRule { premises :: S.HashSet (Assumption Identifier), __ :: Conclusion Identifier }

instance Show UnordRule where
  show = show . toRuleClause

instance Pretty UnordRule where
  pretty = pretty . toRuleClause

fromRuleClause :: RuleClause -> UnordRule
fromRuleClause (Rule prems c) = UnordRule (S.fromList prems) c

toRuleClause :: UnordRule -> RuleClause
toRuleClause (UnordRule prems c) = Rule (S.toList prems) c

unrollRule :: UnordRule -> ImplicitRuleTree
unrollRule (UnordRule prems concl) =
  foldr'
    (\p rt -> Branch p [rt])
    (Result concl)
  prems

{- filter out rules with the proposition as their premise, then filter it from propositions -}
factorPremise :: Assumption Identifier -> [UnordRule] -> (Assumption Identifier, [UnordRule])
factorPremise commonPremise rules =
  let
    factored = rules >>= \(UnordRule prems c) -> do
      -- find all alpha equivalent premises
      premise' <- filter (alphaEq commonPremise) . S.toList $ prems
      let
      -- remove it from the rule
        prems' = S.delete premise' prems
        -- build a rename mapping
        renaming =
          M.fromList $ 
            {- the renaming of old names to new names ensures no name 
               clashing when we have multiple matching premises in one rule
               e.g.
                  rule: P a b, Q a, Q b |- R b
                  commonPremise: Q a
                  factoring (correct):
                    Q a |- P a b |- Q b |- R b
                        |- P b a |- Q a |- R a
                  without the wapping the factoring would become:
                    Q a |- P a b |- Q b |- R b
                        |- P a a |- Q a |- R a
            -}
            zip oldNames newNames ++ zip newNames oldNames 
          where
            oldNames = idArgs premise'
            newNames = idArgs commonPremise
            idArgs = mapMaybe getId . argumentsOf
        -- rename rule's variables such that the premise becomes equal to `commonPremise`
        prems'' = S.map (substituteAll renaming) prems'
        c' = substituteAll renaming c
      return $ UnordRule prems'' c'
  in (commonPremise, factored)

{-
  per rule collect all unique premises up to _alpha_ equality
  then for each signature
  - factor all the premises with the signature out of each rule
  - group rules by shared premises up to alpha equivalence

  wanna do
  => keep merging the tree until you cant any more
-}
buildRuleForest :: [RuleClause] -> ImplicitRuleForest
buildRuleForest rules =
  let
    urules = map fromRuleClause rules
    -- collect all premises up to alpha equivalence
    prems = S.toList . S.map unAlpha . S.unions . map (S.map Alpha . premises) $ urules
    -- factor into its own rule tree
    groupings = map (`factorPremise` urules) prems
    -- collect all premiseless rules
    facts = map mkResult . filter (S.null . premises) $ urules
    trees = map mkBranch groupings
  in RF (facts ++ trees)
  where
    mkBranch (assump, rule) = Branch assump (map unrollRule rule)
    mkResult (UnordRule _ cs) = Result cs

compactify :: P.Program -> ImplicitCompactProgram
compactify program =
  Compact
    { moduleDecl = P.moduleDecl program
    , imports    = P.imports program
    , dataDefs   = P.dataDefs program
    , signatures = P.signatures program
    , ruleForest = buildRuleForest . map snd $ P.rules program
    , ordClauses = P.ordClauses program
    , querries   = P.querries program
    }