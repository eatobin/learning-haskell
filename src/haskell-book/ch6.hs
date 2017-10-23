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

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a

newtype Age =
  Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber = Age
  toNumber (Age n) = n
  defaultNumber = Age 65

newtype Year =
  Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber = Year
  toNumber (Year n) = n
  defaultNumber = Year 1988

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a b = fromNumber summed
  where
    integerOfA = toNumber a
    integerOfB = toNumber b
    summed =
      integerOfA + integerOfB

-- λ> sumNumberish (Year 100) (Year 100)
-- Year 200

sumNumberish' :: Numberish a => a -> a -> a
sumNumberish' a b = fromNumber (toNumber a + toNumber b)

-- λ> sumNumberish' (Year 100) (Year 100)
-- Year 200

sumNumberish'' :: Numberish a => a -> a -> Integer
sumNumberish'' a b = toNumber a + toNumber b

-- λ> sumNumberish'' (Year 100) (Year 100)
-- 200
-- λ> fromNumber 88 :: Age
-- Age 88
-- λ> defaultNumber ::Age
-- Age 65

addWeird :: (Ord a, Num a) => a -> a -> a
addWeird x y =
  if x > 1
  then x + y
  else x

-- The problem is that having a Num constraint (alone) on our type a isn’t
-- enough. Num doesn’t imply Ord.

addWeird' ::  Integer -> Integer -> Integer
addWeird' x y =
  if x > 1
  then x + y
  else x

-- But Integer does imply Ord!

-- page 206

-- 1 c
-- 2 a
-- 3 a
-- 4 d
-- 5 a

newtype Person = Person Bool
                 deriving Show

printPerson :: Person -> IO ()
printPerson = print

-- typechecks after adding Show

data Mood = Blah
          | Woot deriving (Eq, Show)

settleDown :: Mood -> Mood
settleDown x =
  if x == Woot
  then Blah
  else x

-- typechecks after adding Eq

type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

-- s1 = Sentence "dogs" "drool" -- Needs an Object
s2 :: Sentence
s2 = Sentence "Julie" "loves" "dogs"

-- λ> s2
-- Sentence "Julie" "loves" "dogs"
-- deriving Show -- all that's needed

newtype Rocks =
  Rocks String deriving (Eq, Show)

newtype Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

-- phew = Papu "chases" True
-- Nope!

truth :: Papu
truth = Papu (Rocks "chomskydoz")
             (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'

-- Nope - no instance of Ord to compare with
