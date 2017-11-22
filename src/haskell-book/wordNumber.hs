{-# OPTIONS -Wall #-}

module WordNumber where

import Data.List (intersperse)

interspersed :: Bool
interspersed =
  intersperse ',' "abcde" == "a,b,c,d,e"

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord _ = "nine"

digits :: Int -> [Int]
digits n = undefined

wordNumber :: Int -> String
wordNumber n = undefined
