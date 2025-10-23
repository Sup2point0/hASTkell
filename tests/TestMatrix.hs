module TestMatrix where

import Test.Tasty
import Test.Tasty.HUnit (testCase, Assertion)

import Syntax
import Core


t :: Matrix Int
t = Matrix
  [ [1, 2, 3]
  , [4, 5, 6]
  ]


tests_matrix :: TestTree
tests_matrix = testGroup "matrix"
  [ test_collection "properties" test_properties
  ]

test_properties :: [Assertion]
test_properties =
  [ rows t === 2
  , cols t === 3
  ]
