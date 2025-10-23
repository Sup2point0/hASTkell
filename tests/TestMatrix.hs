module TestMatrix where

import Test.Tasty
import Test.Tasty.HUnit

import Syntax
import Core


t = Matrix
  [ [1, 2, 3]
  , [4, 5, 6]
  ] :: Matrix Int

t' = Matrix
  [ [0, 0, 0]
  , [7, 8, 9]
  ] :: Matrix Int


tests_matrix = testGroup "matrix"
  [ test_collection "properties" test_properties
  , test_collection "arithmetic" test_arithmetic
  ] :: TestTree

test_properties =
  [ rows t === 2
  , cols t === 3
  ] :: [Assertion]

test_logic =
  [ t === t
  , t !== Matrix [[]]
  , t !== Matrix [ [0, 0], [0, 0] ]
  ]

test_arithmetic =
  [ t + t' === Matrix [ [1, 2, 3] , [11, 13, 15] ]
  ] :: [Assertion]
