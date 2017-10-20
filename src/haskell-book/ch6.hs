{-# OPTIONS -Wall #-}

module Ch6 where

data HiLo =
  Hi | Lo

instance Eq HiLo where
  (==) Hi Hi = True
  (==) Lo Lo = True
  (==) _ _   = False

newtype MyHand =
  MyHand HiLo

instance Eq MyHand where
  (==) (MyHand hand) (MyHand hand') =
    hand == hand'

data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Show)

data Date =
  Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _     = False

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _   = GT
  compare _ Fri   = LT
  compare _ _     = EQ

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
         weekday == weekday'
         && dayOfMonth == dayOfMonth'

newtype Identity a =
  Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

-- pg 178

newtype TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
 (==) (TisAn x) (TisAn x') = x == x'

-- λ> TisAn 4 == TisAn 4
-- True
-- λ> TisAn 4 == TisAn 46
-- False

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two d e) (Two d' e') =
       d == d' && e == e'

-- λ> Two 4 8 == Two 4 8
-- True
-- λ> Two 4 8 == Two 4 9
-- False

data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt i) (TisAnInt i')     = i == i'
  (==) (TisAString s) (TisAString s') = s == s'
  (==) _ _                            = False

-- λ> TisAnInt 55 == TisAnInt 55
-- True
-- λ> TisAnInt 55 == TisAnInt 559
-- False
-- λ> TisAString "aa" == TisAString "aa"
-- True
-- λ> TisAString "aa" == TisAString "aah"
-- False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair g _) (Pair _ h) = g == h

-- λ> Pair 5 5 == Pair 5 5
-- True
-- λ> Pair 5 5 == Pair 6 6
-- False

data Tuple a b =
   Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple c d) (Tuple c' d') =
       c == c' && d == d'

-- λ> Tuple 6 8 == Tuple 6 8
-- True
-- λ> Tuple 6 8 == Tuple 6 88
-- False

data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne w) (ThatOne x) = w == x
  (==) _ _                     = False

-- λ> ThisOne 'x' == ThatOne 'x'
-- True
-- λ> ThisOne 'x' == ThatOne 'h'
-- False

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello d) (Hello d')     = d == d'
  (==) (Goodbye e) (Goodbye e') = e == e'
  (==) _ _                      = False

-- λ> Hello "dog" == Hello "dog"
-- True
-- λ> Hello "dog" == Hello "dogs"
-- False
-- λ> Goodbye "cat" == Goodbye "cat"
-- True
-- λ> Goodbye "cat" == Goodbye "cats"
-- False
