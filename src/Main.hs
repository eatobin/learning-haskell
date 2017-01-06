module Main where

main :: IO ()
main =
  putStrLn "hello eric"

waxOn :: Int
waxOn =
  x * 5
    where
      z = 7
      y = z + 8
      x = y ^ 2

triple :: Int -> Int
triple x =
  x * 3

waxOff :: Int -> Int
waxOff =
  triple

myString :: String
myString = "Curry is awesome"

curryIsA :: String -> String
curryIsA s = s ++ "!"

curryIsB :: String -> String
curryIsB s = s !! 4

curryIsC :: String -> String
curryIsC s = (drop 9 s) ++ "!"

thirdLetter :: String -> Char
thirdLetter x = x !! 3

letterIndex :: Int -> Char
letterIndex x = myString !! x

rvrs :: String
rvrs =
  drop 9 myString ++ " " ++ take 2 $ drop 6 myString ++ " " ++ take 5 myString


module Reverse where

rvrs :: String -> String
rvrs x = drop 9 x ++ " " ++ take 2 $ drop 6 x ++ " " ++ take 5 x

main :: IO ()
main = print $ rvrs myString
