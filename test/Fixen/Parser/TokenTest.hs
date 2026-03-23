{-# LANGUAGE OverloadedStrings #-}

module Fixen.Parser.TokenTest (tokenTests) where

import Data.List.NonEmpty
import Data.Text
import Error.Diagnose.Position qualified as Diag
import Fixen.IR.Core qualified as Core
import Fixen.Parser.Common
import Fixen.Parser.Token
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec qualified as P

tokenTests :: TestTree
tokenTests =
  testGroup
    "Fixen.Parser.Token"
    [ parseRawLowerHsIdentifierStringTest
    , parseRawCapitalizedHsIdentifierStringTest
    , parseRawAnyCaseHsIdentifierStringTest
    , parseRawOpCharTest
    , parseRawOpIdentifierStringTest
    , parseModuleNameTest
    , parseLowerFirstSimpleIdentifierTest
    , parseLowerFirstFQNTest
    ]

parseRawLowerHsIdentifierStringTest :: TestTree
parseRawLowerHsIdentifierStringTest =
  testGroup
    "parseRawLowerHsIdentifierString"
    [ parserShouldPass parseRawLowerHsIdentifierString "hel_lo1'L" "hel_lo1'L"
    , parserShouldPass parseRawLowerHsIdentifierString "_" "_"
    , parserShouldPass parseRawLowerHsIdentifierString "a b" "a"
    , parserShouldFail (parseRawLowerHsIdentifierString <* P.eof) "a b"
    , parserShouldFail parseRawLowerHsIdentifierString "Hello"
    , parserShouldFail parseRawLowerHsIdentifierString "."
    , parserShouldFail parseRawLowerHsIdentifierString "1h"
    , parserShouldFail parseRawLowerHsIdentifierString "\"h\""
    , parserShouldFail parseRawLowerHsIdentifierString "if"
    ]

parseRawCapitalizedHsIdentifierStringTest :: TestTree
parseRawCapitalizedHsIdentifierStringTest =
  testGroup
    "parseRawCapitalizedHsIdentifierString"
    [ parserShouldPass parseRawCapitalizedHsIdentifierString "Hel_lo'1L" "Hel_lo'1L"
    , parserShouldPass parseRawCapitalizedHsIdentifierString "A b" "A"
    , parserShouldFail (parseRawCapitalizedHsIdentifierString <* P.eof) "A b"
    , parserShouldFail parseRawCapitalizedHsIdentifierString "hello"
    , parserShouldFail parseRawCapitalizedHsIdentifierString "."
    , parserShouldFail parseRawCapitalizedHsIdentifierString "1H"
    , parserShouldFail parseRawCapitalizedHsIdentifierString "\"H\""
    ]

parseRawAnyCaseHsIdentifierStringTest :: TestTree
parseRawAnyCaseHsIdentifierStringTest =
  testGroup
    "parseRawAnyCaseHsIdentifierStringTest"
    [ parserShouldPass parseRawAnyCaseHsIdentifierString "he11_'o" "he11_'o"
    , parserShouldPass parseRawAnyCaseHsIdentifierString "He11_'o" "He11_'o"
    , parserShouldPass parseRawAnyCaseHsIdentifierString "H h" "H"
    , parserShouldPass parseRawAnyCaseHsIdentifierString "h H" "h"
    , parserShouldFail (parseRawAnyCaseHsIdentifierString <* P.eof) "h H"
    , parserShouldFail (parseRawAnyCaseHsIdentifierString <* P.eof) "H h"
    , parserShouldFail parseRawAnyCaseHsIdentifierString "if"
    , parserShouldFail parseRawAnyCaseHsIdentifierString "!"
    , parserShouldFail parseRawAnyCaseHsIdentifierString "1H"
    , parserShouldFail parseRawAnyCaseHsIdentifierString "\"H\""
    ]

parseRawOpCharTest :: TestTree
parseRawOpCharTest =
  testGroup
    "parseRawOpChar"
    $ ((\x -> parserShouldPass parseRawOpChar (pack [x]) x) <$> opChars)
      ++ [ parserShouldFail parseRawOpChar "h"
         , parserShouldFail parseRawOpChar "1"
         , parserShouldFail parseRawOpChar "("
         , parserShouldFail parseRawOpChar "["
         , parserShouldFail parseRawOpChar "H"
         ]

parseRawOpIdentifierStringTest :: TestTree
parseRawOpIdentifierStringTest =
  testGroup
    "parseRawOpIdentifierString"
    [ parserShouldPass parseRawOpIdentifierString "!" "!"
    , parserShouldPass parseRawOpIdentifierString "++" "++"
    , parserShouldPass parseRawOpIdentifierString "+-*" "+-*"
    , parserShouldPass parseRawOpIdentifierString "+-*/" "+-*/"
    , parserShouldPass parseRawOpIdentifierString "\\" "\\"
    , parserShouldPass parseRawOpIdentifierString "->" "->"
    , parserShouldPass parseRawOpIdentifierString "==" "=="
    , parserShouldPass parseRawOpIdentifierString "<=" "<="
    , parserShouldPass parseRawOpIdentifierString "=>><=" "=>><="
    , parserShouldFail parseRawOpIdentifierString "_"
    , parserShouldFail parseRawOpIdentifierString "a"
    , parserShouldFail parseRawOpIdentifierString "1"
    , parserShouldFail parseRawOpIdentifierString "\"a\""
    , parserShouldFail parseRawOpIdentifierString "'a'"
    , parserShouldFail parseRawOpIdentifierString "(a)"
    , parserShouldFail parseRawOpIdentifierString "(+)"
    ]

parseModuleNameTest :: TestTree
parseModuleNameTest =
  testGroup
    "parseModuleName"
    [ parserShouldPass
        parseModuleName
        "Data.List"
        ( Core.ModuleName
            (Diag.Position (1, 1) (1, 10) "test")
            ( Core.SimpleIdentifier
                (Diag.Position (1, 1) (1, 5) "test")
                (pack "Data")
                :| [ Core.SimpleIdentifier
                      (Diag.Position (1, 6) (1, 10) "test")
                      (pack "List")
                   ]
            )
        )
    , parserShouldPass
        parseModuleName
        "MyModule"
        ( Core.ModuleName
            (Diag.Position (1, 1) (1, 9) "test")
            ( Core.SimpleIdentifier
                (Diag.Position (1, 1) (1, 9) "test")
                (pack "MyModule")
                :| []
            )
        )
    , parserShouldPass
        parseModuleName
        "A.B.C"
        ( Core.ModuleName
            (Diag.Position (1, 1) (1, 6) "test")
            ( Core.SimpleIdentifier
                (Diag.Position (1, 1) (1, 2) "test")
                (pack "A")
                :| [ Core.SimpleIdentifier
                      (Diag.Position (1, 3) (1, 4) "test")
                      (pack "B")
                   , Core.SimpleIdentifier
                      (Diag.Position (1, 5) (1, 6) "test")
                      (pack "C")
                   ]
            )
        )
    , parserShouldPass
        parseModuleName
        "Main"
        ( Core.ModuleName
            (Diag.Position (1, 1) (1, 5) "test")
            ( Core.SimpleIdentifier
                (Diag.Position (1, 1) (1, 5) "test")
                (pack "Main")
                :| []
            )
        )
    , parserShouldPass
        parseModuleName
        "Data.Text.Lazy"
        ( Core.ModuleName
            (Diag.Position (1, 1) (1, 15) "test")
            ( Core.SimpleIdentifier
                (Diag.Position (1, 1) (1, 5) "test")
                (pack "Data")
                :| [ Core.SimpleIdentifier
                      (Diag.Position (1, 6) (1, 10) "test")
                      (pack "Text")
                   , Core.SimpleIdentifier
                      (Diag.Position (1, 11) (1, 15) "test")
                      (pack "Lazy")
                   ]
            )
        )
    , parserShouldFail parseModuleName "123Module"
    , parserShouldFail (parseModuleName <* P.eof) "Module-Name"
    , parserShouldFail (parseModuleName <* P.eof) "Module Name"
    , parserShouldFail parseModuleName ""
    , parserShouldFail parseModuleName "data.List"
    , parserShouldFail parseModuleName "Data.list"
    , parserShouldFail parseModuleName ".Data.List"
    , parserShouldFail parseModuleName "Data.List."
    ]

parseLowerFirstSimpleIdentifierTest :: TestTree
parseLowerFirstSimpleIdentifierTest =
  testGroup
    "parseLowerFirstSimpleIdentifier"
    [ parserShouldPass
        parseLowerFirstSimpleIdentifier
        "data"
        ( Core.SimpleIdentifier
            (Diag.Position (1, 1) (1, 5) "test")
            (pack "data")
        )
    , parserShouldPass
        parseLowerFirstSimpleIdentifier
        "myVariable"
        ( Core.SimpleIdentifier
            (Diag.Position (1, 1) (1, 11) "test")
            (pack "myVariable")
        )
    , parserShouldPass
        parseLowerFirstSimpleIdentifier
        "x"
        ( Core.SimpleIdentifier
            (Diag.Position (1, 1) (1, 2) "test")
            (pack "x")
        )
    , parserShouldPass
        parseLowerFirstSimpleIdentifier
        "var_123"
        ( Core.SimpleIdentifier
            (Diag.Position (1, 1) (1, 8) "test")
            (pack "var_123")
        )
    , parserShouldPass
        parseLowerFirstSimpleIdentifier
        "a1b2c3"
        ( Core.SimpleIdentifier
            (Diag.Position (1, 1) (1, 7) "test")
            (pack "a1b2c3")
        )
    , parserShouldFail parseLowerFirstSimpleIdentifier "Data"
    , parserShouldFail parseLowerFirstSimpleIdentifier "123data"
    , parserShouldFail parseLowerFirstSimpleIdentifier ""
    , parserShouldFail (parseLowerFirstSimpleIdentifier <* P.eof) "data-name"
    , parserShouldFail (parseLowerFirstSimpleIdentifier <* P.eof) "data.name"
    , parserShouldFail (parseLowerFirstSimpleIdentifier <* P.eof) "data name"
    ]

parseLowerFirstFQNTest :: TestTree
parseLowerFirstFQNTest =
  testGroup
    "parseLowerFirstFQN"
    [ parserShouldPass
        parseLowerFirstFQN
        "Data.list"
        ( Core.FullyQualifiedName
            (Diag.Position (1, 1) (1, 10) "test")
            ( Core.ModuleName
                (Diag.Position (1, 1) (1, 5) "test")
                ( Core.SimpleIdentifier
                    (Diag.Position (1, 1) (1, 5) "test")
                    (pack "Data")
                    :| []
                )
            )
            (Core.SimpleIdentifier (Diag.Position (1, 6) (1, 10) "test") (pack "list"))
        )
    , parserShouldPass
        parseLowerFirstFQN
        "Data.Set.list'2_"
        ( Core.FullyQualifiedName
            (Diag.Position (1, 1) (1, 17) "test")
            ( Core.ModuleName
                (Diag.Position (1, 1) (1, 9) "test")
                ( Core.SimpleIdentifier
                    (Diag.Position (1, 1) (1, 5) "test")
                    (pack "Data")
                    :| [ Core.SimpleIdentifier (Diag.Position (1, 6) (1, 9) "test") (pack "Set")
                       ]
                )
            )
            (Core.SimpleIdentifier (Diag.Position (1, 10) (1, 17) "test") (pack "list'2_"))
        )
    , parserShouldFail parseLowerFirstFQN "Data.List"
    , parserShouldFail parseLowerFirstFQN "Data"
    , parserShouldFail parseLowerFirstFQN "data"
    , parserShouldFail parseLowerFirstFQN "123Data.List"
    , parserShouldFail parseLowerFirstFQN ""
    , parserShouldFail parseLowerFirstFQN "Data-list"
    , parserShouldFail parseLowerFirstFQN "data.List"
    , parserShouldFail parseLowerFirstFQN "data.list"
    , parserShouldFail (parseLowerFirstFQN <* P.eof) "Data.list.set"
    , parserShouldFail parseLowerFirstFQN ".Data.list"
    ]

parserShouldPass :: (Show a, Eq a) => Parser a -> Text -> a -> TestTree
parserShouldPass p input expected = testCase (show input) $ do
  let result = P.runParser p "test" input
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right res -> res @?= expected

parserShouldFail :: (Show a, Eq a) => Parser a -> Text -> TestTree
parserShouldFail p input = testCase (show input) $ do
  let result = P.runParser p "test" input
  case result of
    Left _ -> return ()
    Right res -> assertFailure $ "Parse succeeded: " ++ show res
