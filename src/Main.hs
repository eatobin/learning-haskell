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
