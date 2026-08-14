module Fixen.Diagnostics
  ( FixenDiagnosticSeverity (..)
  , FixenSourceSpan (..)
  , FixenDiagnostic (..)
  , toFixenDiagnostics
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
import Error.Diagnose
  ( Marker (..)
  , Position (..)
  , Report (Err, Warn)
  , reportsOf
  )
import Fixen.Monad.Type (FixenErrorResult)

data FixenDiagnosticSeverity
  = FixenDiagnosticError
  | FixenDiagnosticWarning
  deriving (Eq, Show)

data FixenSourceSpan = FixenSourceSpan
  { fixenStartLine :: Int
  , fixenStartColumn :: Int
  , fixenEndLine :: Int
  , fixenEndColumn :: Int
  }
  deriving (Eq, Show)

data FixenDiagnostic = FixenDiagnostic
  { fixenDiagnosticSeverity :: FixenDiagnosticSeverity
  , fixenDiagnosticMessage :: Text
  , fixenDiagnosticSpan :: Maybe FixenSourceSpan
  }
  deriving (Eq, Show)

positionToFixenSpan :: Position -> FixenSourceSpan
positionToFixenSpan position =
  case position of
    Position
      (startLine, startColumn)
      (endLine, endColumn)
      _file ->
        FixenSourceSpan
          { fixenStartLine = startLine
          , fixenStartColumn = startColumn
          , fixenEndLine = endLine
          , fixenEndColumn = endColumn
          }

firstMarkerSpan :: [(Position, Marker String)] -> Maybe FixenSourceSpan
firstMarkerSpan markers =
  case primaryPositions of
    firstPrimary : _rest ->
      Just (positionToFixenSpan firstPrimary)

    [] ->
      case markers of
        (firstPosition, _marker) : _rest ->
          Just (positionToFixenSpan firstPosition)

        [] ->
          Nothing
  where
    primaryPositions =
      [ position
      | (position, This _message) <- markers
      ]

reportToFixenDiagnostic :: Report String -> FixenDiagnostic
reportToFixenDiagnostic report =
  case report of
    Err _code message markers _notes ->
      FixenDiagnostic
        { fixenDiagnosticSeverity = FixenDiagnosticError
        , fixenDiagnosticMessage = Text.pack message
        , fixenDiagnosticSpan = firstMarkerSpan markers
        }

    Warn _code message markers _notes ->
      FixenDiagnostic
        { fixenDiagnosticSeverity = FixenDiagnosticWarning
        , fixenDiagnosticMessage = Text.pack message
        , fixenDiagnosticSpan = firstMarkerSpan markers
        }

toFixenDiagnostics :: FixenErrorResult -> [FixenDiagnostic]
toFixenDiagnostics diagnostics =
  map reportToFixenDiagnostic (reportsOf diagnostics)
















