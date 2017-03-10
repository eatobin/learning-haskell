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
  (==) _ _                            = False


data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair g _) (Pair _ h) = g == h


data Tuple a b =
   Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple c d) (Tuple c' d') =
       c == c' && d == d'


data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne w) (ThatOne x) = w == x
  (==) _ _                     = False


data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello d) (Hello d')     = d == d'
  (==) (Goodbye e) (Goodbye e') = e == e'
  (==) _ _                      = False
