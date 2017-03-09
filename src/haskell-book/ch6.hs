{-# OPTIONS -Wall #-}

module Ch6 where

data HiLo =
  Hi | Lo

instance Eq HiLo where
  (==) Hi Hi = True
  (==) Lo Lo = True
  (==) _ _   = False


data Identity a =
  Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'


data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
 (==) (TisAn x) (TisAn x') = x == x'


data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two d e) (Two d' e') =
       d == d' && e == e'


data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt i) (TisAnInt i')     = i == i'
  (==) (TisAString s) (TisAString s') = s == s'
