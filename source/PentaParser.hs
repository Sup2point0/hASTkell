module PentaParser where

import Core
import Preprocess (sanitise)


parse :: String -> (Tree String)
parse str = parse1 (sanitise str) ""

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
      _     -> parse3 right ""

parse3 :: String -> String -> (Tree String)
parse3 left right
  = case right of
    ('^':rest) -> Tree (parse left) Exp (parse rest)
    _ -> case left of
      (_:_) -> parse3 (init left) (last left : right)
      _     -> Leaf right
