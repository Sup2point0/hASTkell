module Types.Matrix where


data Matrix t = Matrix [[t]]
  deriving Show


instance (Eq t) => Eq (Matrix t) where
  m@(Matrix entries) == m'@(Matrix entries')
    = (
        rows m == rows m'
        && cols m == cols m'
        && and (map (and . uncurry (zipWith (==))) (zip entries entries'))
      )

instance (Num t) => Num (Matrix t) where
  negate (Matrix entries)
    = Matrix (map (map negate) entries)

  abs (Matrix entries)
    = Matrix (map (map abs) entries)

  signum (Matrix entries)
    = Matrix (map (map signum) entries)
  
  (Matrix entries) + (Matrix entries')
      = Matrix entries''
    where
      entries'' = map (uncurry (zipWith (+))) (zip entries entries')
  
  fromInteger n = Matrix [[fromInteger n]]


rows :: (Matrix t) -> Int
rows (Matrix entries) = length entries

cols :: (Matrix t) -> Int
cols (Matrix entries) = maximum (map length entries)

transpose :: (Matrix t) -> (Matrix t)
transpose (Matrix entries) = Matrix (transpose' entries)
  where
    transpose' :: [[t]] -> [[t]]
    transpose' [] = []
    transpose' entries = zipped : transpose' rest
      where (zipped, rest) = foldr zip' ([], []) entries

    zip' :: [t] -> ([t], [[t]]) -> ([t], [[t]])
    zip' [] acc = acc
    zip' (x:xs) (zipped, rest) = (x:zipped, xs:rest)
