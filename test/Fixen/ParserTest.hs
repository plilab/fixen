import Test.Tasty

import Fixen.Parser.TokenTest

main :: IO ()
main =
  defaultMain $
    testGroup
      "Fixen.Parser"
      [ tokenTests
      ]
