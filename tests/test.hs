import Test.Tasty

import TestMatrix
import TestPreprocess


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "hASTkell"
  [ tests_matrix
  , tests_preprocess
  ]
