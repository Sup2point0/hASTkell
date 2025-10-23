import Test.Tasty

import TestPreprocess


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "hASTkell"
  [ tests_preprocess
  ]
