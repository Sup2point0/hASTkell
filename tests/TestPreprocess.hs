module TestPreprocess where

import Test.Tasty
import Test.Tasty.HUnit

import Syntax
import Preprocess


tests_preprocess :: TestTree
tests_preprocess = testGroup "preprocess"
  [ test_collection "strip_spaces" test_strip_spaces
  ]

test_strip_spaces :: [Assertion]
test_strip_spaces =
  [ strip_spaces "" === ""
  , strip_spaces " " === ""
  , strip_spaces "test" === "test"
  , strip_spaces " test " === "test"
  , strip_spaces "t e s t" === "test"
  ]
