module Tree where

data Tree a = Children [Tree a] | Leaf a deriving (Show)
