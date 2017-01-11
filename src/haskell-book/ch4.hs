{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

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
  if (x >= 0)
    then x
    else - x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f t1 t2 =
  ((snd t1, snd t2), (fst t1, fst t2))

x = (+)

add1 :: String -> Int
add1 xs =
  w + 1
    where w = length xs
