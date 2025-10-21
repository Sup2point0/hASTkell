module Core where


strip_spaces :: String -> String
strip_spaces str
  = [char | char <- str, char /= ' ']