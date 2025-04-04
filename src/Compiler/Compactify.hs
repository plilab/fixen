module Compiler.Compactify where

import Common.Util
import Data.Foldable (Foldable(foldr'), find)
import Data.Maybe (mapMaybe)
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Syntax.Common
import Syntax.Raw
import Syntax.Compact
import Control.Arrow (Arrow(second))
import Data.Traversable (for)

data UnordRule = UnordRule { premises :: S.HashSet (Assumption Identifier), conclusion :: Conclusion Identifier }

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
    factoredRules <- for rules $ \(UnordRule prems c) -> do
      -- find an alpha equivalent premise
      premise' <- find (alphaEq commonPremise) prems 
      let
      -- remove it from the rule
        prems' = S.delete premise' prems
        -- build a rename mapping:
        renaming = M.fromList $ zip (idArgs commonPremise) (idArgs premise')
          where idArgs = mapMaybe getId . argumentsOf
        -- rename rule's variables such that the premise becomes equal to `commonPremise`
        prems'' = S.map (substituteAll renaming) prems'
        c' = substituteAll renaming c
      return $ UnordRule prems'' c'
    
    return (commonPremise, factoredRules)

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
    urules = map (\(Rule prems c) -> UnordRule (S.fromList prems) c) rules

    groupings = mapMaybe (\(Signature name _) -> factorPremise name urules) signts

    trees = map (second $ map unrollRule) groupings

  in RF trees

compactify :: RawProgram -> CompactProgram Identifier
compactify (RawProgram decls) =
  let signs = mapMaybe getSignature decls
  in Compact
    { dataDefs   = mapMaybe getDataDef decls
    , signatures = signs
    , ruleForest = buildRuleForest signs $ mapMaybe getRule decls
    , ordClauses = mapMaybe getOrd decls
    , querries   = mapMaybe getQuery decls
    }