module Core where


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
