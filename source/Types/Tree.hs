{-# LANGUAGE GADTs #-}
module Types.Tree where

import Types.Operator


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
