module Types.Matrix where


data Matrix t = Matrix [[t]]
  deriving Show


identity :: (Num t) => Int -> Matrix t
identity n
  | n < 0     = error "Cannot create identity matrix with negative dimensions"
  | n == 0    = Matrix []
  | otherwise = Matrix [
      [if (j == i) then 1 else 0 | j <- [1..n] ]
    | i <- [1..n]
    ]


instance (Eq t) => Eq (Matrix t) where
  (Matrix []) == (Matrix []) = True
  (Matrix []) == _           = False
  _           == (Matrix []) = False

  m@(Matrix cells) == m'@(Matrix cells')
    = (
        rows m == rows m'
        && cols m == cols m'
        && and (map (and . uncurry (zipWith (==))) (zip cells cells'))
      )


instance Functor (Matrix) where
  fmap f (Matrix []) = Matrix []
  fmap f (Matrix cells) = Matrix (map (map f) cells)


instance (Num t) => Num (Matrix t) where
  negate = fmap negate
  abs    = fmap abs   
  signum = fmap signum
  
  (Matrix cells) + (Matrix cells') = Matrix (
      map (uncurry (zipWith (+))) (zip cells cells')
    )
  
  -- k * (Matrix cells)
  --   = Matrix (map (map (k*)) cells)
  
  fromInteger n = Matrix [[fromInteger n]]


raw :: (Matrix t) -> [[t]]
raw (Matrix cells) = cells

rows :: (Matrix t) -> Int
rows (Matrix cells) = length cells

cols :: (Matrix t) -> Int
cols (Matrix cells) = maximum (map length cells)

is_square :: (Matrix t) -> Bool
is_square mat = (rows mat == cols mat)


transpose :: (Matrix t) -> (Matrix t)
transpose (Matrix cells) = Matrix (transpose' cells)
  where
    transpose' :: [[t]] -> [[t]]
    transpose' [] = []
    transpose' cells' = zipped : transpose' rest
      where (zipped, rest) = foldr zip' ([], []) cells'

    zip' :: [t] -> ([t], [[t]]) -> ([t], [[t]])
    zip' [] acc = acc
    zip' (x:xs) (zipped, rest) = (x:zipped, xs:rest)

invert :: (Num t) => (Matrix t) -> (Matrix t)
invert (Matrix []) = error
invert mat
    | is_square mat = inv
    | otherwise     = error "Cannot invert a non-square matrix"
  where
    aug = _join_ mat (identity (rows mat))
    add = Matrix [
        r1,
        zipWith (uncurry (+)) r1 r2
      ]
    (r1 : r2 : _) = raw aug
    inv = add


_join_ :: (Matrix t) -> (Matrix t) -> (Matrix t)
_join_ (Matrix cells) (Matrix cells') = Matrix (
    map (uncurry (++)) (zip cells cells')
  )
