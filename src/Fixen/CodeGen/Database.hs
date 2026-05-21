{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultilineStrings #-}

-- |
-- Module      : Fixen.CodeGen.Database
-- Description : Code Generation for Fact Databases
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module provides code-generation facilities for the fact database and
-- associated utilities.
--
-- The 'codeGenDb' function generates:
--
-- 1. The @Database@ type ('codeGenDbDef')
-- 2. The @emptyDb@ definition ('codeGenEmptyDb')
-- 3. The @|=@ (entailment) definition ('codeGenEntailment')
-- 4. The @insertToDb@ function for fact insertion ('codeGenFactInsertions')
--
-- @since 0.0.1
module Fixen.CodeGen.Database where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Fixen.CodeGen.Common
import Fixen.Fields
import Fixen.IR.AST
import Fixen.IR.RelationRepresentation
import Fixen.Monad
import Fixen.Utils
import Prelude hiding (map)

--------------------------------------------------------------------------------

-- * Main Entry Point

--------------------------------------------------------------------------------

-- | Generates the Haskell source code for dealing with fact databases.
--
-- @since 0.0.1
codeGenDb :: RelationRepresentation -> FixenPass CodeGenState Text
codeGenDb r = do
  let db_def = codeGenDbDef r
      empty_db_def = codeGenEmptyDb r
  entailment_def <- codeGenEntailment r
  insert_def <- codeGenInsert r
  return $
    Text.intercalate "\n\n"
      [db_def, empty_db_def, entailment_def, insert_def]


--------------------------------------------------------------------------------

-- * Helpers

--------------------------------------------------------------------------------

-- ** Database-Type Generation

-- | Generates the @Database@ type.
--
-- @since 0.0.1
codeGenDbDef :: RelationRepresentation -> Text
codeGenDbDef r =
  let facts = Map.toList r
      header = 
        """
        ----- FACT DATABASE -----
        data Database = Database
          { 
        """
      db_fields = Text.intercalate "\n  , " (codeGenDbField <$> facts)
      end = "\n  } deriving Eq"
   in Text.concat [header, db_fields, end]

-- | Generates an individual field in the @Database@ type.
--
-- @since 0.0.1
codeGenDbField 
  :: (Text, RelationRepresentationInfo)  
  -- ^ The 'Text' is the relation name, the 'RelationRepresentationInfo' is its
  -- representation information.
  --
  -- @since 0.0.1
  -> Text
codeGenDbField (t, r) =
  let db_ty_code =
        r ^. database . types
          & buildDbFieldType
          & codeGenType
   in Text.concat [dbFactSelector t, " :: ", db_ty_code]

-- | Builds the 'Type' of the database representation.
--
-- @since 0.0.1
buildDbFieldType
  :: [(QueryType, StoreType, Type)]
  -- ^ The relation representation's types.
  -> Type
-- If the relation has no arguments, just use a bool.
buildDbFieldType [] = buildSimpleType "Bool"
buildDbFieldType [(LatticeMeet {}, _, x)] = x
buildDbFieldType ((LatticeMeet {}, _, t) : x : xs) =
  -- build a tuple of those stuff.
  let remaining_types = thrd <$> (x :| xs)
   in TypeTuple (-1) t remaining_types
-- The last thing should be a hashset of the argument's type
buildDbFieldType [(_, _, x)] = TypeApp (-1) (buildSimpleType "HashSet") x
-- At any point, the moment we see a partially ordered type, just use a HashSet
-- and store everything remaining in a tuple
buildDbFieldType ((Meet _ _, _, t) : x : xs) =
  let remaining_types = thrd <$> (x :| xs)
   in TypeApp 
        (-1) 
        (buildSimpleType "HashSet") 
        (TypeTuple (-1) t remaining_types)
-- Otherwise, just use a HashMap.
buildDbFieldType ((Match, _, t) : xs) = 
  TypeApp 
    (-1) 
    (TypeApp (-1) (buildSimpleType "HashMap") t) 
    (buildDbFieldType xs)

-- ** Empty Database

-- | Generates the definition of @emptyDb@.
--
-- @since 0.0.1
codeGenEmptyDb :: RelationRepresentation -> Text
codeGenEmptyDb r =
  let header =
        """
        emptyDb :: Database
        emptyDb = Database
          { 
        """
      all_fields =
        Map.toList r
          <&> codeGenEmptyDbFields
          & Text.intercalate "\n  , "
   in Text.concat [header, all_fields, "\n  }"]

-- | Generates the field of an @emptyDb@.
--
-- @since 0.0.1
codeGenEmptyDbFields 
  :: (Text, RelationRepresentationInfo)
  -- ^ The 'Text' is the relation name, and the 'RelationRepresentationInfo'
  -- is its information.
  --
  -- @since 0.0.1
  -> Text
codeGenEmptyDbFields (t, r) =
  let field_name = dbFactSelector t
   in Text.append field_name $ 
        case r ^. database . types of
          [] -> " = False"
          [(LatticeMeet _ _ _ b, _, _)] -> Text.append " = " $ codeGenIdentifier b
          ls@((LatticeMeet {}, _, _) : _) ->
            let remaining_bots = (\case
                  (LatticeMeet _ _ _ b, _,_) -> codeGenIdentifier b
                  _ -> error "non lattice argument comes after lattice argument!") 
                    <$> ls
                tup = parenthesize $ Text.intercalate ", " remaining_bots
             in Text.append " = " tup
          [_] -> " = HashSet.empty"
          ((Meet _ _, _, _) : _) -> " = HashSet.empty"
          _ -> " = HashMap.empty"

-- ** Entailment

-- | Generates the @|=@ function.
--
-- @since 0.0.1
codeGenEntailment :: RelationRepresentation -> FixenPass CodeGenState Text
codeGenEntailment rep = do
  let ty_decl = 
        """
        infix 0 |=

        (|=) :: Database -> Fact -> Bool
        """
      all_patterns = Map.toList rep
  all_patterns_code <- mapM codeGenEntailmentCase all_patterns
  return $ Text.intercalate "\n" (ty_decl : all_patterns_code)

-- | Generates a case for @|=@.
--
-- @since 0.0.1
codeGenEntailmentCase 
  :: (Text, RelationRepresentationInfo)
  -- ^ The 'Text' is the name of the relation, and the 'RelationRepresentationInfo'
  -- is its information.
  -> FixenPass CodeGenState Text
codeGenEntailmentCase (rel_name, rep_info)
  | null db_ty = return header
  | (LatticeMeet {}, _, _) : _ <- db_ty
    = return header
  | length steps_individual_code <= 1 =
    return $ Text.concat (header : steps_individual_code)
  | otherwise =
    return $ Text.concat 
      [ header
      , "fromMaybe False $ do\n        "
      , Text.intercalate "\n        " (init steps_individual_code)
      -- put the return $ on the last guy! We do not put the return $ in the
      -- steps function in case there is only one step to run; in that case,
      -- there is no need to use return.
      , "\n        return $ ", last steps_individual_code
      ]
  where
    -- The database types
    db_ty = rep_info ^. database . types

    -- The header of the pattern:
    -- Either:
    --   db |= MyRel = _factsMyRel db
    -- if MyRel has no arguments (i.e., just a Bool), otherwise,
    -- if MyRel only has lattice arguments
    --   db |= (MyRel _v0 ...) = 
    --     let (_t0, ...) = _factsMyRel db
    --      in leq0 _v0 _t0 && leq1 _v1 _t1 && ...
    -- otherwise:
    --   db |= (MyRel _v0 _v1 ...) =
    --     let db' = _factsMyRel db
    --      in 
    header
      | null db_ty = Text.concat ["db |= ", rel_name, " = ", dbFactSelector rel_name , " db"]
      | ((LatticeMeet {}, _, _) : _) <- db_ty
        = let leqs = 
                fmap
                  ( \(l', _, _) -> case l' of
                      Match -> "(==)" -- technically impossible, since we
                                      -- are in a set of partially ordered
                                      -- terms
                      Meet x _ -> codeGenIdentifier x -- technically impossible
                                        -- since we are in lattice-only land
                      LatticeMeet x _ _ _ -> codeGenIdentifier x
                  )
                  db_ty
              idxed_leqs = zip [0 .. length leqs - 1] leqs
              conds = fmap
                        (\(i, l) -> Text.intercalate " " [l, v i, t i]) idxed_leqs
              lhs_tup = 
                if length db_ty == 1 
                  then t 0 
                  else parenthesize (Text.intercalate ", " $ t <$> [0 .. length db_ty - 1])
           in Text.concat [
                  "db |= ", case_pattern, " =\n",
                  "  let ", lhs_tup, " = ", dbFactSelector rel_name, " db\n",
                  "   in ", Text.intercalate " && " conds
                ]
      | otherwise 
        = Text.concat
            [ "db |= ", case_pattern, " =\n"
            , "  let db' = ", dbFactSelector rel_name, " db\n"
            , "   in "
            ]

    -- The pattern of this case, e.g., (MyRel _v0 _v1).
    case_pattern = parenthesize $ Text.intercalate " " (rel_name : case_vars)

    -- The variables of this case.
    case_vars = v <$> [0 .. length db_ty - 1]

    -- Generates _v0, _v1, etc, for any i.
    v :: Int -> Text
    v = Text.append "_v" ∘ Text.show

    -- Generates _t0, _t1, etc, for any i.
    t :: Int -> Text
    t = Text.append "_t" ∘ Text.show

    -- The extraction order is just the values of the extraction map, in that 
    -- order.
    extraction_order = values $ rep_info ^. database . map 

    -- The extraction procedure is then specified by the extraction order and the
    -- database types. Essentially, we have a list of i elements, where each
    -- element has:
    -- 1. An index j that corresponds to where the ith fact argument is in the
    --    database
    -- 2. The query type
    -- 3. The type of the argument.
    extraction_proc = zipWith (\x (y, _, z) -> (x, y, z)) extraction_order db_ty

    -- The code for all the steps for determining entailment.
    steps_individual_code = steps 0 extraction_proc

    -- This function generates each individual step for determining entailment.
    -- The idea is that we descend the database, matching on HashMap keys,
    -- then further descend into the nested HashMap/HashSet.
    --
    -- Note that none of the code is indented. Indentation is handled in the
    -- main function body.
    steps :: Int -> [(Int, QueryType, Type)] -> [Text]
    -- Case that doesn't happen anyway.
    steps _ [] = [] 
    -- We reached a HashSet of partially ordered stuff.
    -- Just check if any of the elements in the HashSet subsume the corresponding
    -- variables. It looks like
    --   any (\(t0, t1, ...) -> leq4 v4 t0 && leq0 v0 t1 && ...) step{step_no}
    steps step_no ls@((idx, Meet leq_function _, _) : _) =
      let n = length ls -- how big is this tuple? is it standalone?
          curr_db = getCurrDb step_no
       in if n == 1
            then -- HashSet (ty). Just look for anything inside that subsumes
                 --the var
              [Text.concat 
                 ["any (", codeGenIdentifier leq_function, " ", v idx, ") ", curr_db]
              ]
            else -- HashSet (ty1, ty2, ...). Use `any` to look through stuff.
                 -- the idea is that we will use
                 --   any (\(t0, t1, ...) -> leq3 t0 v3 && leq2 t1 v2 ...)
                 -- where the v numbers are given in idx.
              let -- just a series of variables _t0 to _t{n - 1}
                  fn_params = t <$> [0 .. n - 1]
                  -- the _v{x} variables
                  remaining_vars = (\(i, _, _) -> v i) <$> ls
                  remaining_vars_leq =
                    fmap
                      ( \(_, l', _) -> case l' of
                          Match -> "(==)" -- technically impossible, since we
                                          -- are in a set of partially ordered
                                          -- terms
                          Meet x _ -> codeGenIdentifier x
                          LatticeMeet x _ _ _ -> codeGenIdentifier x
                      )
                      ls
                  -- now we build the \(t0, t1, ... ) -> function header
                  fn_arg_tup = parenthesize $ Text.intercalate ", " fn_params
                  fn_header = Text.concat ["\\", fn_arg_tup, " -> "]

                  -- the components are just a series of conjunctions of leqs.
                  fn_body_components =
                    zipWith3 
                      (\t_var v_var leq' -> 
                          parenthesize 
                            (Text.intercalate " " [leq', v_var, t_var])) 
                      fn_params 
                      remaining_vars 
                      remaining_vars_leq
                  fn_body = Text.intercalate " && " fn_body_components

                  -- now we finally have the function
                  any_fn = parenthesize $ Text.concat [fn_header, fn_body]
               in [Text.concat ["any ", any_fn, " ", curr_db]]
    -- We reached a HashSet of partially ordered stuff, again. This is essentially
    -- the same exact case as the one before.
    -- Just check if any of the elements in the HashSet subsume the corresponding
    -- variables. It looks like
    --   any (\(t0, t1, ...) -> leq4 v4 t0 && leq0 v0 t1 && ...) step{step_no}
    steps step_no ls@((idx, LatticeMeet leq_function _ _ _, _) : _) =
      let n = length ls -- how big is this tuple? is it standalone?
          curr_db = getCurrDb step_no
       in if n == 1
            then -- HashSet (ty). Just look for anything inside that subsumes
                 --the var
              [Text.concat 
                 ["any (", codeGenIdentifier leq_function, " ", v idx, ") ", curr_db]
              ]
            else -- HashSet (ty1, ty2, ...). Use `any` to look through stuff.
                 -- the idea is that we will use
                 --   any (\(t0, t1, ...) -> leq3 t0 v3 && leq2 t1 v2 ...)
                 -- where the v numbers are given in idx.
              let -- just a series of variables _t0 to _t{n - 1}
                  fn_params = t <$> [0 .. n - 1]
                  -- the _v{x} variables
                  remaining_vars = (\(i, _, _) -> v i) <$> ls
                  remaining_vars_leq =
                    fmap
                      ( \(_, l', _) -> case l' of
                          Match -> "(==)" -- technically impossible, since we
                                          -- are in a set of partially ordered
                                          -- terms
                          Meet x _ -> codeGenIdentifier x
                          LatticeMeet x _ _ _ -> codeGenIdentifier x
                      )
                      ls
                  -- now we build the \(t0, t1, ... ) -> function header
                  fn_arg_tup = parenthesize $ Text.intercalate ", " fn_params
                  fn_header = Text.concat ["\\", fn_arg_tup, " -> "]

                  -- the components are just a series of conjunctions of leqs.
                  fn_body_components =
                    zipWith3 
                      (\t_var v_var leq' -> 
                          parenthesize 
                            (Text.intercalate " " [leq', v_var, t_var])) 
                      fn_params 
                      remaining_vars 
                      remaining_vars_leq
                  fn_body = Text.intercalate " && " fn_body_components

                  -- now we finally have the function
                  any_fn = parenthesize $ Text.concat [fn_header, fn_body]
               in [Text.concat ["any ", any_fn, " ", curr_db]]
    -- HashSet of some discrete stuff. Just check element membership.
    steps step_no [(idx, Match, _)] =
      [Text.concat [v idx, " `HashSet.member` ", getCurrDb step_no]]
    -- lookup from the hashmap and immediately destructure
    steps step_no ((idx, Match, _) : xs@((_, LatticeMeet {}, _) : _)) =
      let lhs_tup = 
            if length xs == 1 
              then t 0 
              else parenthesize (Text.intercalate ", " $ t <$> [0 .. length xs - 1])
          curr_db = getCurrDb step_no
          curr_step = 
            Text.concat 
              [lhs_tup, " <- ", curr_db, " HashMap.!? ", v idx]
          idxes =
            fmap
              (\(i, x, _) -> 
                case x of
                  Match -> (i, "==")
                  Meet l _ -> (i, codeGenIdentifier l)
                  LatticeMeet l _ _ _ -> (i, codeGenIdentifier l)
              )
              xs
          conds =
            fmap
              (\(tidx, (vidx, l)) ->
                Text.intercalate " " [l, v vidx, t tidx]
              )
              (zip [0 .. length xs] idxes)
       in [curr_step, Text.intercalate " && " conds]
    -- lookup from the hashmap
    steps step_no ((idx, Match, _) : xs) =
      let curr_db = getCurrDb step_no
          curr_step = 
            Text.concat 
              [getCurrDb (step_no + 1), " <- ", curr_db, " HashMap.!? ", v idx]
       in curr_step : steps (step_no + 1) xs

    -- Finds out what the current map/set we have descended into is.
    getCurrDb 0 = "db'" -- have not started descending
    getCurrDb n = Text.append "step" (Text.show n) -- have already descended

-- ** Fact Insertion

-- | Generates the @insertToDb@ function.
--
-- @since 0.0.1
codeGenInsert :: RelationRepresentation -> FixenPass CodeGenState Text
codeGenInsert rep = do
  let ty_decl = 
        """
        insertToDb :: Database -> Fact -> Maybe Database
        insertToDb db fact
          | db |= fact = Nothing
        """
      all_cases = Map.toList rep
  all_cases_code <- mapM codeGenInsertCase all_cases
  return $ Text.intercalate "\n" (ty_decl : all_cases_code)

-- Generates a case for the @insertToDb@ function.
--
-- @since 0.0.1
codeGenInsertCase
  :: (Text, RelationRepresentationInfo)
  -- ^ The 'Text' is the relation's name
  -> FixenPass CodeGenState Text
codeGenInsertCase (rel_name, rep_info)
  | null db_ty = return header
  | only_lattices =
      let lhs_tup = 
            if length db_ty == 1
              then t 0
              else parenthesize $ Text.intercalate ", " $ t <$> [0 .. length db_ty - 1]
          idxed_db_ty = zip [0 .. length db_ty - 1] db_ty
          inserted_fact = 
            case idxed_db_ty of
              [(_, (LatticeMeet _ j _ _, _, _))] ->
                Text.intercalate " " [codeGenIdentifier j, t 0, v 0]
              _ -> parenthesize $ Text.intercalate ", " $
                      fmap
                        (\(i, (x, _, _)) ->
                            case x of
                              LatticeMeet _ j _ _ -> 
                                Text.intercalate " " [codeGenIdentifier j, t i, v i]
                              _ -> error "non-lattice argument found after lattice argument!"
                        )
                        idxed_db_ty
       in return $ Text.concat [
            "insertToDb db ", case_pattern, " =\n",
            "  let ", lhs_tup, " = ", dbFactSelector rel_name, " db\n",
            "      new_fact = ", inserted_fact, "\n",
            "   in Just db { ", dbFactSelector rel_name, " = new_fact }"
          ]
  | otherwise = return $ Text.concat [header, singleton_fact_code, new_mp_code]
  where
    db_ty = rep_info ^. database . types

    -- The header of the case. If the relation has no arguments, it is just
    --   insertToDb db MyRel = Just db { _factsMyRel = True }
    -- since the representation of the relation is just Bool.
    -- Otherwise, it will be something like 
    --   insertToDb db (MyRel _v0 _v1 ...) =
    --     let mp = _factsMyRel db
    --         new_fact =
    -- in which we will proceed to generate:
    --   1. The fact database that stores the new fact only
    --   2. The code that performs a union of this singleton map with one fact
    --      and the current database.
    header =
      if null db_ty
        then
          Text.concat [ "insertToDb db ", rel_name, " = Just db { ", 
                        dbFactSelector rel_name, " = True }" ]
        else
          Text.concat
            [ "insertToDb db ", case_pattern, " =\n"
            , "  let mp = ", dbFactSelector rel_name, " db\n"
            , "      new_fact = "
            ]

    only_lattices = case db_ty of
      (LatticeMeet {}, _, _) : _ -> True
      _ -> False

    -- The pattern (MyRel _v0 _v1 ... _vn)
    case_pattern = parenthesize $ Text.intercalate " " (rel_name : case_vars)

    -- The variables of the pattern, _v0, _v1, ..., _vn
    case_vars = v <$> [0 .. length db_ty - 1]
    -- The order in which the fact arguments appear in the database
    insertion_order = values (rep_info ^. database . map)

    -- The sequence of steps for inserting the fact into the database
    extraction_proc = zipWith (\x (y, _, z) -> (x, y, z)) insertion_order db_ty

    -- Generates the singleton map/set containing the fact to be inserted
    singleton_fact_code = singleton_steps extraction_proc

    -- Individual steps for building the singleton map/set containing the fact
    -- to be inserted
    singleton_steps :: [(Int, QueryType, Type)] -> Text
    -- no one cares since this never happens anyway
    singleton_steps [] = ""
    -- arrived at a sequence of partially ordered stuff. Put it in a
    -- singleton HashSet as a tuple.
    singleton_steps ls@((idx, Meet _ _, _) : _) =
      if length ls == 1
        then -- just a hashset
          Text.concat ["HashSet.singleton ", v idx]
        else
          let remaining_vars = fmap (\(i, _, _) -> v i) ls
              tuple = parenthesize $ Text.intercalate ", " remaining_vars
           in Text.concat ["HashSet.singleton ", tuple]
    -- arrived at a sequence of lattices. Put it in a tuple.
    singleton_steps ls@((idx, LatticeMeet {}, _) : _) =
      if length ls == 1
        then v idx -- just the thing itself -- a hashset
          
        else
          let remaining_vars = fmap (\(i, _, _) -> v i) ls
              tuple = parenthesize $ Text.intercalate ", " remaining_vars
           in tuple
    -- arrived at the rightmost thing. simply put it as a singleton in the hashset
    singleton_steps [(idx, _, _)] = Text.concat ["HashSet.singleton ", v idx]
    -- arrived at some discrete hashmap key.
    singleton_steps ((idx, _, _) : xs) =
      Text.concat ["HashMap.singleton ", v idx, " ", parenthesize (singleton_steps xs)]

    -- The code that unions the singleton map/set with the existing database.
    new_mp_code =
      Text.concat
        [ "\n      mp' = ", insertionFn 0 extraction_proc
        , "\n              new_fact"
        , "\n              mp"
        , "\n   in Just db { ", dbFactSelector rel_name, " = mp' }"
        ]

    -- Generates the union function between the singleton map/set with the
    -- existing fact database.
    --
    -- The first argument is the indentation of the current step
    insertionFn :: Int -> [(Int, QueryType, Type)] -> Text
    -- no one cares; this case never happens anyway!
    insertionFn _ [] = ""
    -- arrived at some partially ordered stuff. the insertion function looks
    -- something like:
    --   (\s1 s2 -> HashSet.union s1 (HashSet.filter (\... -> ...) s2))
    -- where the filter function removes all the stuff in s2 that is strictly
    -- subsumed by the new fact.
    --
    -- TODO. Handle the fact that we actually have to take MLBs of partial ords
    -- and joins of lattices, if they are present.
    insertionFn indent ls@((idx, Meet l _, _) : _) =
      let n = length ls
       in if n == 1
            then
              -- singleton. do a basic union, eliminating stuff in the original
              -- database that the new fact strictly subsumes
              -- consider deleting this branch
              Text.concat
                [ "(\\s1 s2 ->", indentation (indent + 2)
                , "HashSet.union", indentation (indent + 3)
                , "s1", indentation (indent + 3)
                , "(HashSet.filter", indentation (indent + 4)
                , "(\\_t -> not (_t /= ",  v idx, " && ", codeGenIdentifier l, " _t ", v idx, "))"
                , indentation (indent + 4)
                , "s2))"
                ]
            else
              -- tuple. Same thing as before, do a union and eliminate stuff in
              -- the original database that the new fact strictly subsumes.
              let db_vars = t <$> [0 .. n - 1]
                  filter_fn_hd =
                    Text.concat
                      [ indentation (indent + 4)
                      , "(\\", parenthesize $ Text.intercalate ", " db_vars
                      , " -> not ("
                      ]
                  remaining_vars = fmap (\(i, _, _) -> v i) ls
                  remaining_vars_leq =
                    fmap
                      ( \(_, l', _) -> case l' of
                          Match -> "(==)"
                          Meet x _ -> codeGenIdentifier x
                          LatticeMeet x _ _ _ -> codeGenIdentifier x
                      )
                      ls
                  filter_fn_body_components = 
                    zipWith3 
                      (\t'' v' leq' -> Text.intercalate " " [t'', "/=", v', "&&", leq', t'', v']) 
                      db_vars
                      remaining_vars
                      remaining_vars_leq
                  filter_fn_body = Text.intercalate " && " filter_fn_body_components
                  -- now we have the filter function which allows us to perform
                  -- the union
               in Text.concat
                      [ "(\\s1 s2 ->", indentation (indent + 2)
                      , "HashSet.union", indentation (indent + 3)
                      , "s1", indentation (indent + 3)
                      , "(HashSet.filter", filter_fn_hd, filter_fn_body, "))"
                      , indentation (indent + 4), "s2))"
                      ]
    insertionFn _ ls@((_, LatticeMeet _ j _ _, _) : _) =
      let n = length ls
       in if n == 1
            then codeGenIdentifier j 
            else
              -- tuple. do an N-wise join.
              let fst_tup = parenthesize $ Text.intercalate ", " $ t <$> [0 .. n - 1]
                  snd_tup = parenthesize $ Text.intercalate ", " $ t' <$> [0 .. n - 1]
                  remaining_vars_join =
                    fmap
                      ( \(_, l', _) -> case l' of
                          Match -> error "discrete variable appearing after lattice argument"
                          Meet _ _ -> error "partial ord appearing after lattice argument"
                          LatticeMeet _ j' _ _ -> j'
                      )
                      ls
                  idxd_remaining_vars = zip [0 .. n - 1] remaining_vars_join
                  body =
                    parenthesize $ Text.intercalate ", " $ fmap
                      (\(i, j') -> Text.intercalate " " [codeGenIdentifier j', t i, t' i])
                      idxd_remaining_vars
               in Text.concat
                      [ "\\", fst_tup, " ", snd_tup, " -> ", body]

    -- Rightmost discrete argument. Just union.
    insertionFn _ [(_, Match, _)] = "HashSet.union"
    -- HashMap key. Just use HashMap.unionWith
    insertionFn indent (_ : xs) = 
      Text.concat
        [ "HashMap.unionWith", indentation (indent + 1), 
          parenthesize $ insertionFn (indent + 1) xs ]

    -- Generates indentation for some code
    indentation 0 = " "
    indentation n = Text.cons '\n' $ Text.replicate (12 + (n * 2)) " "

    -- Generates _v0, _v1, etc, for any i.
    v :: Int -> Text
    v = Text.append "_v" ∘ Text.show

    -- Generates _t0, _t1, etc, for any i.
    t :: Int -> Text
    t = Text.append "_t" ∘ Text.show

    -- Generates _t'0, _t'1, etc, for any i.
    t' :: Int -> Text
    t' = Text.append "_t'" ∘ Text.show
