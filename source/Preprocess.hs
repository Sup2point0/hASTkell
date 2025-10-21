module Preprocess where


sanitise :: String -> String
sanitise = pad_negative . strip_spaces

pad_negative :: String -> String
pad_negative str@('-':_)
  = '0' : str
pad_negative str = str

strip_spaces :: String -> String
strip_spaces str
  = [char | char <- str, char /= ' ']
