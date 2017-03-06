{-# OPTIONS -Wall #-}

module Arith3Broken where

main :: IO ()
main = do
  print ((1 :: Int) + (2 :: Int))
  putStrLn "10"
  print (negate (-1 :: Int))
  print ((+) (0 :: Int) blah)
    where blah = negate 1

