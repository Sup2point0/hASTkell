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


data Operator = Plus | Minus | Mult | Div | Exp

instance Show Operator where
  show Plus = " + "
  show Minus = " - "
  show Mult = " * "
  show Div = " / "
  show Exp = " ^ "


parse :: String -> (Tree String)
parse str = parse1 (strip_spaces str) ""

parse1 :: String -> String -> (Tree String)
parse1 left right
  = case right of
    ('+':rest) -> Tree (parse left) Plus (parse rest)
    ('-':rest) -> Tree (parse left) Minus (parse rest)
    _ -> case left of
      (_:_) -> parse1 (init left) (last left : right)
      _     -> parse2 right ""

parse2 :: String -> String -> (Tree String)
parse2 left right
  = case right of
    ('*':rest) -> Tree (parse left) Mult (parse rest)
    ('/':rest) -> Tree (parse left) Div (parse rest)
    _ -> case left of
      (_:_) -> parse2 (init left) (last left : right)
      _     -> Leaf right
