module Types.Matrix where


data Matrix t = Matrix [[t]]


instance (Eq t) => Eq (Matrix t) where
  m@(Matrix entries) == m'@(Matrix entries')
    = (
        rows m == rows m'
        && cols m == cols m'
      )


rows :: Matrix t -> Int
rows (Matrix entries) = length entries

cols :: Matrix t -> Int
cols (Matrix entries) = maximum (map length entries)
