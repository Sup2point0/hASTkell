module Types.Matrix where


data Matrix t = Matrix [[t]]
  deriving Show


instance (Eq t) => Eq (Matrix t) where
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
  negate (Matrix cells)
    = Matrix (map (map negate) cells)

  abs (Matrix cells)
    = Matrix (map (map abs) cells)

  signum (Matrix cells)
    = Matrix (map (map signum) cells)
  
  (Matrix cells) + (Matrix cells')
      = Matrix cells''
    where
      cells'' = map (uncurry (zipWith (+))) (zip cells cells')
  
  -- k * (Matrix cells)
  --   = Matrix (map (map (k*)) cells)
  
  fromInteger n = Matrix [[fromInteger n]]


rows :: (Matrix t) -> Int
rows (Matrix cells) = length cells

cols :: (Matrix t) -> Int
cols (Matrix cells) = maximum (map length cells)

transpose :: (Matrix t) -> (Matrix t)
transpose (Matrix cells) = Matrix (transpose' cells)
  where
    transpose' :: [[t]] -> [[t]]
    transpose' [] = []
    transpose' cells = zipped : transpose' rest
      where (zipped, rest) = foldr zip' ([], []) cells

    zip' :: [t] -> ([t], [[t]]) -> ([t], [[t]])
    zip' [] acc = acc
    zip' (x:xs) (zipped, rest) = (x:zipped, xs:rest)
