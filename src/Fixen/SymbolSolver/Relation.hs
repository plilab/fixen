module Fixen.SymbolSolver.Relation where

import Control.Lens
import Data.Map.Strict qualified as Map
import Fixen.IR.AST
import Fixen.Monad
import Fixen.SymbolSolver.Common
import Fixen.SymbolSolver.Extern
import Fixen.SymbolSolver.Validation

initEnvWithRelation :: SymbolEnv -> Relation -> FixenPass SymbolState SymbolEnv
initEnvWithRelation env r = do
  _ <- validateRelation r env
  env
    & insertRelationParamTypesAsDiscrete
    & insertRelation
    & foldMWith initEnvWithExternSymbol non_p_ord_params
  where
    rel_name = simpleIdentifier $ nameOf r
    rel_params = relationParams r

    insertRelation e =
      let new_rel_info =
            RelationInfo
              { _relationDeclaration = r
              , _relationArgMatchInfo = Prelude.replicate (Prelude.length rel_params) Unmatched
              }
       in case e ^. infoMap . relationInfoMap . at rel_name of
            Just _ -> e
            Nothing -> e & infoMap . relationInfoMap . at rel_name ?~ new_rel_info

    insertRelationParamTypesAsDiscrete e =
      let info = calculateRepresentativeFromType <$> rel_params
       in Prelude.foldl' softInsertTypeAsDiscrete e info

    softInsertTypeAsDiscrete :: SymbolEnv -> Representative -> SymbolEnv
    softInsertTypeAsDiscrete e p =
      -- check if the discreteness annotation is already there.
      -- if it is, don't re-insert; it might be partially ordered!
      case e ^. infoMap . relationParamKindInfoMap . at p of
        Just _ -> e
        Nothing ->
          e
            & infoMap
              . relationParamKindInfoMap
              . at p
              ?~ Discrete

    non_p_ord_params =
      let -- filter out all the args that are partial ords, since their DB representation
          -- will not have partial ords (only the underlying type). If the underlying type is
          -- the same name (or references it), they are already in the extern list.
          non_p_ord_args = Prelude.filter notAPartialOrd rel_params
       in getAllTypeNamesList non_p_ord_args

    notAPartialOrd (TypeName _ (IdentifierSimpleIdentifier (SimpleIdentifier _ s))) =
      let partial_ords = env ^. infoMap . partialOrdInfoMap
       in s `Map.notMember` partial_ords
    notAPartialOrd _ = True

validateRelation :: SymbolValidator Relation
validateRelation = validate rules
  where
    rules = [againstOtherRelations, againstPartialOrd, againstExtern, againstPrelude]
    againstOtherRelations =
      validateNamed
        (validateAgainstRelation "another rel with the same name")
    againstPartialOrd =
      validateNamed
        ( validateAgainstPartialOrd
            "rel declaration"
            relationValidationErrorNotes
        )
    againstExtern =
      validateNamed
        ( validateAgainstExtern "rel declaration" relationValidationErrorNotes
        )
    againstPrelude =
      validateNamed (validateAgainstPreludeCapitalized "rel declaration" "rel declaration")
