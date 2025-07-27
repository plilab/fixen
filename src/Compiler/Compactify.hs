{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE TupleSections #-}
module Compiler.Compactify ( compactify ) where

import Common.Util
import Data.Foldable (Foldable(foldr'))
import Data.Maybe (mapMaybe)
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Syntax.Common
import qualified Syntax.Sorted as P
import Syntax.Compact
import Prettyprinter
import Data.List (nub)

type ContRuleClause = Rule (Premise Var) ContinuationFact
data UnordRule 
  = UnordRule 
  { assumptions :: S.HashSet (Assumption Var)
  , _conditions  :: S.HashSet (Expr Var)
  , _conts :: ContinuationFact
  }

instance Show UnordRule where
  show = show . toRuleClause

instance Pretty UnordRule where
  pretty = pretty . toRuleClause

fromRuleClause :: ContRuleClause -> UnordRule
fromRuleClause (Rule prems c) =
  UnordRule
    (S.fromList $ mapMaybe getAssumption prems)
    (S.fromList $ mapMaybe getCondition prems)
    c

toRuleClause :: UnordRule -> ContRuleClause
toRuleClause (UnordRule assumps conds c) =
  Rule ((Assumed <$> S.toList assumps) ++ (Condition <$> S.toList conds)) c

unrollRule :: UnordRule -> ImplicitRuleTree
unrollRule (UnordRule assumps conds concl) =
  foldr'
    (\a rt -> Branch (Assumed a) [rt])
    (foldr' 
      (\c rt -> Branch (Condition c) [rt])
      (Result concl)
    conds)
  assumps

{- filter out rules with the proposition as their premise, then filter it from propositions -}
factorPremise :: Assumption Var -> [UnordRule] -> (Premise Var, [UnordRule])
factorPremise commonPremise rules =
  let
    factored = rules >>= \(UnordRule prems conds c) -> do
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
                  without the swapping the factoring would become:
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
        conds' = S.map (substituteAll renaming) conds
      return $ UnordRule prems'' conds' c'
  in (Assumed commonPremise, factored)

{-
  per rule collect all unique premises up to _alpha_ equality
  then for each signature
  - factor all the premises with the signature out of each rule
  - group rules by shared premises up to alpha equivalence

  wanna do
  => keep merging the tree until you cant any more
-}
buildRuleForest :: [ContRuleClause] -> ImplicitRuleForest
buildRuleForest rules =
  let
    urules = map fromRuleClause rules
    -- collect all premises up to alpha equivalence
    prems = S.toList . S.map unAlpha . S.unions . map (S.map Alpha . assumptions) $ urules
    -- factor into its own rule tree
    groupings = map (`factorPremise` urules) prems
    -- collect all premiseless rules
    facts = map mkResult . filter (S.null . assumptions) $ urules
    trees = map mkBranch groupings
  in RF (facts ++ trees)
  where
    mkBranch (assump, rule) = Branch assump (map unrollRule rule)
    mkResult (UnordRule _ _ cs) = Result cs

continuationalize :: [Signature] -> (Identifier, RuleClause) -> ((Identifier, Continuation Var), ContRuleClause)
continuationalize signs (name, Rule prems concl) = let
  ctx  = nub $ concatMap collectBindings prems
  cont = Cont ctx concl
  rul' = Rule prems $ Proposition name (fst <$> ctx)
  in ((name, cont), rul')
  where
    {- collects all the variables bound in the rule and their type.
      since variables must occur in at least one assumption 
      (as we do no constraint solving), there is nothing to
      collect in `Condition` case -}
    collectBindings (Condition _) = []
    collectBindings (Assumed (Assumption p args)) = 
      mapMaybe
        (\(arg, typ) -> (,typ) <$> getId arg) 
        (zip args $ lookupSignature p signs)

compactify :: P.Program -> ImplicitCompactProgram
compactify program =
  let continuationalized = continuationalize (P.signatures program) <$> P.rules program
  in Compact
    { moduleDecl    = P.moduleDecl program
    , imports       = P.imports program
    , dataDefs      = P.dataDefs program
    , signatures    = P.signatures program
    , ruleForest    = buildRuleForest . map snd $ continuationalized
    , continuations = M.fromList . map fst $ continuationalized
    , priorities    = P.priorities program
    , querries      = P.querries program
    }