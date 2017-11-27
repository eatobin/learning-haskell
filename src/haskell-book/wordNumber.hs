{-# OPTIONS -Wall #-}

module WordNumber where

import Data.List (intersperse, intercalate)

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

-- λ> digitToWord 5
-- "five"

digits :: Int -> [Int]
digits n =
  go (divMod n 10)
  where
    go (0, x) = [x]
    go (xs, x) = go (divMod xs 10) ++ [x]

-- λ> digits 1234
-- [1,2,3,4]
-- λ> digits 1022
-- [1,0,2,2]
-- λ> digits 1
-- [1]
-- λ> digits 0
-- [0]

wordNumber :: Int -> String
-- wordNumber = concat . intersperse "-" . map digitToWord . digits
wordNumber = intercalate "-" . map digitToWord . digits

-- λ> wordNumber 12324546
-- "one-two-three-two-four-five-four-six"
