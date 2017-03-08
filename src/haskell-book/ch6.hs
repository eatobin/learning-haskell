{-# OPTIONS -Wall #-}

module Ch6 where

data Trivial =
  Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True
  Trivial' /= Trivial' = False
