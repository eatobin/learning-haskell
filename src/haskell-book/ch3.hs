{-# OPTIONS -Wall #-}

module Ch3 where

myString :: String
myString = "Curry is awesome"

curryIsA :: String -> String
curryIsA s = s ++ "!"

curryIsB :: String -> Char
curryIsB s = s !! 4

curryIsC :: String -> String
curryIsC s = drop 9 s ++ "!"

thirdLetter :: String -> Char
thirdLetter x = x !! 3

letterIndex :: Int -> Char
letterIndex x = myString !! x

rvrs :: String
rvrs =
  drop 9 myString ++ " " ++ take 2 (drop 6 myString) ++ " " ++ take 5 myString
