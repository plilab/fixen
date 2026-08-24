{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

-- |
--     Module      : Fixen.Parser.Error
--     Description : Custom error datatype for the Fixen parser.
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     This module provides utilities for Fixen parser errors.
--
-- @since 26.8
module Fixen.Parser.Error where

import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Void
import Error.Diagnose (
  Diagnostic,
  Marker (..),
  Note (..),
  Position (..),
  Report (..),
  addReport,
 )
import Text.Megaparsec (
  ErrorFancy (..),
  ErrorItem (..),
  MonadParsec (..),
  ParseError (..),
  ParseErrorBundle (..),
  Pos,
  Token,
  pstateSourcePos,
  reachOffset,
  region,
  showErrorItem,
  sourceColumn,
  sourceLine,
  sourceName,
  tokensLength,
  unPos,
 )

--------------------------------------------------------------------------------

-- * Main Types

--------------------------------------------------------------------------------

-- | The type of parse stacks. Ordered bottom-up, i.e., the first
-- element of the list is the bottom-most stack frame. If viewed as a top-down
-- parser, reading this list from left to right is essentially reading from
-- top down the parse tree.
type ParseStack = [Nonterminal]

-- | Nonterminal symbols are just strings.
type Nonterminal = String

-- | A custom parser error datatype. Mirrors 'ParseError', except with
-- additional bookkeeping data.
--
-- @since 26.8
data FixenParseError
  = FixenTrivialParseError
      (Maybe (ErrorItem (Token Text)))
      -- ^ The unexpected tokens, if any
      (Set (ErrorItem (Token Text)))
      -- ^ The expected tokens
      ParseStack
      -- ^ The parse stack
      [Note String]
      -- ^ A list of hints
  | FixenIndentationError
      Ordering
      -- ^ The ordering
      Pos
      -- ^ Reference Level
      Pos
      -- ^ Actual level
      ParseStack
      -- ^ The parse stack
      [Note String]
      -- ^ A list of hints
  | FixenCustomError
      (Maybe Int)
      -- ^ A potential token length
      ParseStack
      -- ^ The parse stack
      String
      -- ^ The error message
      [Note String]
      -- ^ A list of hints
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- * Dealing with Parser Errors

-- $parserErrors
-- Megaparsec defaults are generally not sufficient for detailed error reporting.
-- To endow the default errors with more information, you have the following
-- options:
-- 1. 'withNote' allows you to add custom 'Note's to errors emitted from a parser
-- 2. 'inContext' allows you to add a stack frame to errors emitted from a parser
-- 3. 'customErrorWithOffset' is similar to 'Text.Megaparsec.customError',
--    except that you can specify the starting offset of the error.

--------------------------------------------------------------------------------

-- | Adds a 'Note' to error(s) emitted from a parser.
--
-- @since 26.8
withNote
  :: MonadParsec FixenParseError Text m
  => Note String
  -- ^ The note to add
  -> m a
  -- ^ The parser
  -> m a
withNote x = region (addNote x)

-- | Adds a 'Nonterminal' (stack frame) to the 'ParseStack' of the error(s)
-- emitted from a parser.
--
-- @since 26.8
inContext :: MonadParsec FixenParseError Text m => Nonterminal -> m a -> m a
inContext x = region (prependFrame x)

-- | Throws a 'FixenCustomError' with a given offset.
-- Essentially the same as 'Text.Megaparsec.customError'.
--
-- @since 26.8
customErrorWithOffset
  :: MonadParsec e s m
  => Int
  -- ^ The offset
  -> e
  -- ^ The custom error
  -> m a
customErrorWithOffset start = parseError . FancyError start . Set.singleton . ErrorCustom

--------------------------------------------------------------------------------

-- * Interop with Diagnose

--------------------------------------------------------------------------------

-- | A modification to 'Error.Diagnose.Compat.Megaparsec.diagnosticFromBundle'.
-- It converts Megaparsec errors and 'FixenParserError's into well-formatted
-- 'Diagnostic's ready to be shown.
--
-- @since 26.8
parserErrorBundleToDiagnostic
  :: Maybe String
  -- ^ An optional error code
  -> ParseErrorBundle Text FixenParseError
  -- ^ The bundle to create a diagnostic from
  -> Diagnostic String
parserErrorBundleToDiagnostic code ParseErrorBundle {..} =
  foldl addReport mempty (asReport . normalizeParseError <$> bundleErrors)
  where
    asReport :: ParseError Text FixenParseError -> Report String
    asReport e@(FancyError start errs) =
      let real_errs = customErrors errs
          parse_stack = getParseStack e
          parse_stack_msg = parseStackMessage parse_stack
       in case real_errs of
            [] -> error "parse error is empty"
            [x] ->
              let err_len = parseErrorTokenLength x
                  err_pos = getPositionFromOffsets start err_len
                  err_msg = parseErrorMessage x
                  err_notes = parseErrorNotes x
               in Err
                    code
                    "syntax error"
                    [(err_pos, This err_msg)]
                    (parse_stack_msg ++ err_notes)
            xs ->
              let package =
                    ( \(i, x) ->
                        let err_len = parseErrorTokenLength x
                            err_pos = getPositionFromOffsets start err_len
                            err_msg = parseErrorMessage x
                            err_notes = parseErrorNotes x
                         in ( [(err_pos, This $ withErrorNumber i err_msg)]
                            , fmap (fmap (withErrorNumber i)) err_notes
                            )
                    )
                      <$> Prelude.zip [1 .. Prelude.length xs] xs
                  (markers, notes) =
                    foldl'
                      (\(x1, x2) (y1, y2) -> (x1 ++ y1, x2 ++ y2))
                      ([], [])
                      package
               in Err code "syntax error" markers (parse_stack_msg ++ notes)
    asReport _ = error "unreachable" -- impossible after normalization
    withErrorNumber :: Int -> String -> String
    withErrorNumber i s = show i ++ ". " ++ s

    customErrors :: Set (ErrorFancy FixenParseError) -> [FixenParseError]
    customErrors s =
      mapMaybe (\case ErrorCustom x -> Just x; _ -> Nothing) $
        Set.toList s
    getPositionFromOffsets :: Int -> Int -> Position
    getPositionFromOffsets start len =
      let (_, pos) = reachOffset start bundlePosState
          src_start = pstateSourcePos pos
          (_, pos') = reachOffset (start + len) pos
          src_end = pstateSourcePos pos'
          file_name = sourceName src_start
          startPos =
            both'
              (fromIntegral . unPos)
              (sourceLine src_start, sourceColumn src_start)
          endPos =
            both'
              (fromIntegral . unPos)
              (sourceLine src_end, sourceColumn src_end)
       in Position startPos endPos file_name
    both' :: (a -> b) -> (a, a) -> (b, b)
    both' f ~(x, y) = (f x, f y)

--------------------------------------------------------------------------------

-- * Helpers

--------------------------------------------------------------------------------

-- | Obtains the error message from a 'FixenParseError'
--
-- @since 26.8
parseErrorMessage :: FixenParseError -> String
-- On trivial parse errors, the expected tokens are found in the notes/hints,
-- see parseErrorNotes.
parseErrorMessage (FixenTrivialParseError Nothing _ _ _) = "unknown parse error"
parseErrorMessage (FixenTrivialParseError _ ex _ _) | Set.null ex = "unknown parse error"
parseErrorMessage (FixenTrivialParseError (Just unex) _ _ _) = "unexpected " ++ showErrorItem textProxy unex
parseErrorMessage (FixenIndentationError {}) = "incorrect indentation"
parseErrorMessage (FixenCustomError _ _ msg _) = msg

-- | Obtains the 'Note's from a 'FixenParseError'.
--
-- @since 26.8
parseErrorNotes :: FixenParseError -> [Note String]
parseErrorNotes (FixenTrivialParseError _ ex _ hs) =
  let expected_msg =
        [ Note $
            "expecting "
              ++ (orList . NonEmpty.fromList . Set.toAscList)
                (showErrorItem textProxy `Set.map` ex)
        | not $ Set.null ex
        ]
   in expected_msg ++ hs
  where
    -- straight plagiarized from Megaparsec
    orList :: NonEmpty String -> String
    orList (x :| []) = x
    orList (x :| [y]) = x <> " or " <> y
    orList xs = intercalate ", " (NonEmpty.init xs) <> ", or " <> NonEmpty.last xs
parseErrorNotes (FixenIndentationError o ref act _ hs) =
  -- essentially plagiarized from Megaparsec
  Note
    ( "got "
        ++ Prelude.show (unPos act)
        ++ ", should be "
        ++ p
        ++ Prelude.show (unPos ref)
        ++ ")"
    )
    : hs
  where
    p = case o of LT -> "less than "; EQ -> "equal to "; GT -> "greater than "
parseErrorNotes (FixenCustomError _ _ msg h) = Note msg : h

-- | Converts a 'ParseStack' trace into 'Note's.
--
-- @since 26.8
parseStackMessage :: ParseStack -> [Note String]
parseStackMessage [] = []
parseStackMessage stack =
  [ Note $
      "error occurred while parsing " ++ intercalate ", " stack
  ]

-- | Obtains the token length from a 'FixenParseError'.
--
-- @since 26.8
parseErrorTokenLength :: FixenParseError -> Int
parseErrorTokenLength (FixenTrivialParseError (Just (Tokens unex)) _ _ _) =
  tokensLength textProxy unex
parseErrorTokenLength (FixenCustomError (Just i) _ _ _) = i
parseErrorTokenLength _ = 1 -- default value, uninteresting

-- | Gets the parse stack from parse errors. This crashes
-- when errors have different non-empty call stacks.
--
-- @since 26.8
getParseStack :: ParseError Text FixenParseError -> [String]
getParseStack (TrivialError {}) = []
getParseStack (FancyError _ s) =
  let stacks =
        Set.map
          ( \case
              ErrorCustom (FixenTrivialParseError _ _ st _) -> st
              ErrorCustom (FixenIndentationError _ _ _ st _) -> st
              ErrorCustom (FixenCustomError _ st _ _) -> st
              _ -> []
          )
          s
      nonempty_stacks = Set.filter (not . Prelude.null) stacks
   in if
        | Set.size nonempty_stacks > 1 -> error "parse errors at the same parse point has different parse stacks!"
        | Set.null nonempty_stacks -> []
        | otherwise -> Set.elemAt 0 nonempty_stacks

-- | Prepends a 'Nonterminal' onto a parse error.
--
-- @since 26.8
prependFrame
  :: Nonterminal
  -- ^ The nonterminal to prepend
  -> ParseError Text FixenParseError
  -- ^ The error
  -> ParseError Text FixenParseError
prependFrame s e =
  let e' = normalizeParseError e
   in case e' of
        FancyError i errs ->
          FancyError i $
            Set.map
              ( \case
                  ErrorCustom (FixenTrivialParseError unex ex st h) ->
                    ErrorCustom (FixenTrivialParseError unex ex (s : st) h)
                  ErrorCustom (FixenIndentationError o p1 p2 st h) ->
                    ErrorCustom (FixenIndentationError o p1 p2 (s : st) h)
                  ErrorCustom (FixenCustomError start st msg h) ->
                    ErrorCustom (FixenCustomError start (s : st) msg h)
                  x -> x
              )
              errs
        x -> x

-- | Adds a note to a parse error.
--
-- @since 26.8
addNote
  :: Note String
  -- ^ The note to add
  -> ParseError Text FixenParseError
  -- ^ The parse error
  -> ParseError Text FixenParseError
addNote note e =
  let e' = normalizeParseError e
   in case e' of
        FancyError i errs ->
          FancyError i $
            Set.map
              ( \case
                  ErrorCustom (FixenTrivialParseError unex ex st h) -> ErrorCustom (FixenTrivialParseError unex ex st (note : h))
                  ErrorCustom (FixenIndentationError o p1 p2 st h) -> ErrorCustom (FixenIndentationError o p1 p2 st (note : h))
                  ErrorCustom (FixenCustomError start st msg h) -> ErrorCustom (FixenCustomError start st msg (note : h))
                  x -> x
              )
              errs
        x -> x

-- | Normalizes parse errors emitted from megaparsec (and Fixen) into a
-- canonical form which we can work with. Essentially, all megaparsec-emitted
-- errors are stored as 'FixenCustomError's. Merging errors from alternatives
-- behaves as we would normally expect. This crashes whenever errors have
-- different non-empty stack traces.
--
-- @since 26.8
normalizeParseError :: ParseError Text FixenParseError -> ParseError Text FixenParseError
normalizeParseError (TrivialError i m s) =
  -- just convert it into a FixenTrivialParseError
  FancyError i . Set.singleton . ErrorCustom $ FixenTrivialParseError m s [] []
normalizeParseError err@(FancyError i s) =
  -- we need to do a proper merging of FixenTrivialParseErrors like we do for
  -- megaparsec's TrivialErrors.
  let (trivials, nontrivials) =
        Set.partition
          ( \case
              ErrorCustom (FixenTrivialParseError {}) -> True
              _ -> False
          )
          s
      -- get the parse stack
      parse_stack = getParseStack err
      -- convert the nontrivial errors into FixenCustomErrors
      converted_nontrivials =
        Set.map
          ( \case
              ErrorFail msg ->
                ErrorCustom $
                  FixenCustomError Nothing parse_stack msg []
              ErrorIndentation o p1 p2 ->
                ErrorCustom $
                  FixenIndentationError o p1 p2 parse_stack []
              e -> e
          )
          nontrivials
      -- we are lazy. so, we're going to merge trivial errors using megaparsec's
      -- TrivialError semigroup capabilities
      base_trivials =
        ( \case
            ErrorCustom (FixenTrivialParseError m ss _ _) ->
              TrivialError @Text @Void i m ss
            _ -> error "unreachable"
        )
          <$> Set.toList trivials
      -- get the (unique) notes we have added to any of these trivial errors
      trivials_notes =
        nub $
          concatMap
            ( \case
                ErrorCustom (FixenTrivialParseError _ _ _ h) -> h
                _ -> error "unreachable"
            )
            (Set.toList trivials)
      -- combine the trivial errors using <>
      combined_trivials = case base_trivials of
        [] -> Set.empty
        (x : xs) -> case Prelude.foldl' (<>) x xs of
          -- map them back to FixenTrivialParseErrors
          TrivialError _ m ss ->
            Set.singleton . ErrorCustom $
              FixenTrivialParseError m ss parse_stack trivials_notes
          _ -> error "unreachable"
   in if not $ Set.null converted_nontrivials
        then FancyError i converted_nontrivials -- prefer nontrivials
        else FancyError i combined_trivials

-- | Quite possibly the most uninteresting thing you will find.
--
-- @since 26.8
textProxy :: Proxy Text
textProxy = Proxy
