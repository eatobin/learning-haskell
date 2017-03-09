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
