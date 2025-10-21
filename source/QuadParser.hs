module QuadParser where


data Tree t where
  Leaf :: t -> Tree t
  Tree :: (Tree t) -> Operator -> (Tree t) -> (Tree t)

instance (Show t) => Show (Tree t) where
  show (Leaf value)
    = show value

  show (Tree left oper right)
    =    "[" ++ show left ++ "]"
      ++ show oper
      ++ "[" ++ show right ++ "]"


data Operator = Plus | Minus | Mult | Div

instance Show Operator where
  show Plus = " + "
  show Minus = " - "
  show Mult = " * "
  show Div = " / "


parse :: String -> (Tree String)
parse str = parse' (strip_spaces str) ""

parse' :: String -> String -> (Tree String)
parse' left right
    = case right of
      ('+':rest) -> split_with Plus rest
      ('-':rest) -> split_with Minus rest
      _ -> case left of
        (_:_) -> parse' (init left) (last left : right)
        _     -> Leaf right
  where
    split_with :: Operator -> String -> (Tree String)
    split_with oper rest
      = Tree (parse left) oper (parse rest)
