module Compiler.Compactify where

import Common.Util
import Data.Foldable (Foldable(foldr'), find)
import Data.Maybe (mapMaybe)
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Syntax.Common
import qualified Syntax.Sorted as P
import Syntax.Compact
import Control.Arrow (Arrow(second))
import Prettyprinter

data UnordRule = UnordRule { premises :: S.HashSet (Assumption Identifier), conclusion :: Conclusion Identifier }

instance Show UnordRule where
  show = show . toRuleClause

instance Pretty UnordRule where
  pretty = pretty . toRuleClause

fromRuleClause :: RuleClause -> UnordRule
fromRuleClause (Rule prems c) = UnordRule (S.fromList prems) c

toRuleClause :: UnordRule -> RuleClause
toRuleClause (UnordRule prems c) = Rule (S.toList prems) c

unrollRule :: UnordRule -> RuleTree Identifier
unrollRule (UnordRule prems concl) = 
  foldr' 
    (\p rt -> RT $ Branch p [rt]) 
    (RT $ Result [concl])
  prems

{- filter out rules with the proposition as their premise, then filter it from propositions -}
factorPremise :: Identifier -> [UnordRule] -> Maybe (Assumption Identifier, [UnordRule])
factorPremise name rules = do 
  -- find the first rule that has a `name` premise
  r0 <- find (any ((name ==) . headSymbolOf) . premises) rules
  -- identify the common premise
  commonPremise <- find ((name ==) . headSymbolOf) $ premises r0
    -- for every rule
  let 
    factor = mapMaybe $ \(UnordRule prems c) -> do
      -- find an alpha equivalent premise
      premise' <- find (alphaEq commonPremise) prems 
      let
      -- remove it from the rule
        prems' = S.delete premise' prems
        -- build a rename mapping:
        renaming = M.fromList $ zip (idArgs premise') (idArgs commonPremise)
          where idArgs = mapMaybe getId . argumentsOf
        -- rename rule's variables such that the premise becomes equal to `commonPremise`
        prems'' = S.map (substituteAll renaming) prems'
        c' = substituteAll renaming c
      return $ UnordRule prems'' c'
  return (commonPremise, factor rules)

-- NOTE: we forget any premiseless rules!
--   => they should be added to the initial DB before computing the fixed point
{- NOTE: ignoring for now:
  - internal dependence on the top level
    e.g. P x x needs to be accounted for on the top level: ```P x x' |- x == x' |- ...``` 
-}
{- Actually wanna do:
  - per rule collect all unique premises up to _literal_ equality
  - then for each signature
    - factor all the premises with the signature out of each rule
    - group rules by shared premises up to alpha equivalence (dont forget to rename in the rest of the rule!)

  => keep merging the tree until you cant any more
-}
buildRuleForest :: [Signature] -> [RuleClause] -> RuleForest Identifier
buildRuleForest signts rules =
  let
    urules = map fromRuleClause rules

    groupings = mapMaybe (\(Signature name _) -> factorPremise name urules) signts

    trees = map (second $ map unrollRule) groupings

  in RF trees

compactify :: P.Program -> CompactProgram Identifier
compactify program =
  let signs = P.signatures program
  in Compact
    { dataDefs   = P.dataDefs program
    , signatures = signs
    , ruleForest = buildRuleForest signs $ map snd $ P.rules program
    , ordClauses = P.ordClauses program
    , querries   = P.querries program
    }