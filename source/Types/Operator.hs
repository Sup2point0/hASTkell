module Types.Operator where


data Operator = Plus | Minus | Mult | Div | Exp

instance Show Operator where
  show Plus = " + "
  show Minus = " - "
  show Mult = " * "
  show Div = " / "
  show Exp = " ^ "
