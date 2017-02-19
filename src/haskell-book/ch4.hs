{-# OPTIONS -Wall #-}

module Ch4 where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood    _ = Blah

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x =
  x == reverse x

myAbs :: Integer -> Integer
myAbs x =
  if x >= 0
    then x
    else - x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f t1 t2 =
  ((snd t1, snd t2), (fst t1, fst t2))

z :: Int -> Int -> Int
z = (+)

add1 :: String -> Int
add1 xs =
  w `z` 1
    where w = length xs

myId :: a -> a
myId x =
  x

myFirst :: [Integer] -> Integer
myFirst (x:_) =
  x
myFirst [] = 0

firstTup :: (a, b) -> a
firstTup =
  fst
