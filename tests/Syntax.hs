module Syntax where

import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase, Assertion, HasCallStack)


infix 1 ===
(===) :: (Show t, Eq t, HasCallStack) => t -> t -> Assertion
(===) = (@?=)


test_collection :: String -> [Assertion] -> TestTree
test_collection name tests
  = testGroup name (zipWith (\n -> testCase ("#" ++ show n)) [1..] tests)
