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
  [ test_collection "constructors" test_constructors
  , test_collection "properties" test_properties
  , test_collection "logic" test_logic
  , test_collection "arithmetic" test_arithmetic
  , test_collection "operations" test_operations
  ] :: TestTree

test_constructors =
  [ identity 0 === Matrix []
  , identity 1 === Matrix [[1]]
  , identity 2 === Matrix [ [1, 0], [0, 1] ]
  , identity 3 === Matrix [ [1, 0, 0], [0, 1, 0], [0, 0, 1] ]
  ] :: [Assertion]

test_properties =
  [ raw t === [ [1, 2, 3], [4, 5, 6] ]

  , rows t === 2
  , cols t === 3
  ] :: [Assertion]

test_logic =
  [ t === t
  , t !== Matrix []
  , t !== Matrix [ [0, 0], [0, 0] ]
  , identity 1 === identity 1
  , identity 1 !== identity 2
  ] :: [Assertion]

test_arithmetic =
  [       -t === Matrix [ [-1, -2, -3], [-4, -5, -6] ]
  ,    abs t === t
  , abs (-t) === t

  , signum   t   === Matrix [ [1, 1, 1], [1, 1, 1] ]
  , signum   t'  === Matrix [ [0, 0, 0], [1, 1, 1] ]
  , signum (-t') === Matrix [ [0, 0, 0], [-1, -1, -1] ]

  ,    t + t' === Matrix [ [1, 2, 3] , [11, 13, 15] ]
  ] :: [Assertion]

test_operations =
  [ fmap id t === t
  , fmap (\x -> x * x) t === Matrix [ [1, 4, 9], [16, 25, 36] ]

  , transpose (Matrix [[0]]) === Matrix [[0]]
  , transpose (Matrix [ [0], [1] ]) === Matrix [[0, 1]]
  , transpose t === Matrix [ [1, 4], [2, 5], [3, 6] ]

  , _join_ t t' === Matrix [ [1, 2, 3, 0, 0, 0], [4, 5, 6, 7, 8, 9] ]

  , invert t === identity 3
  ] :: [Assertion]
