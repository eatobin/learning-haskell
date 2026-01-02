{-# OPTIONS -Wall #-}

module FizzBuzz where

fizzBuzz :: Integer -> String
fizzBuzz x
  | mod x 15 == 0 = "FizzBuzz"
  | mod x 3 == 0 = "Fizz"
  | mod x 5 == 0 = "Buzz"
  | otherwise = show x

fizzBuzzMap :: [String]
fizzBuzzMap =
  map fizzBuzz [7 .. 16]
