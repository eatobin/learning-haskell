{-# OPTIONS -Wall #-}

module Arith4 where

-- id :: a -> a
-- id x = x

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 = read . show

main :: IO ()
main = do
  print (roundTrip 4)
  print (id 4)

main' :: IO ()
main' = do
  print (roundTrip' 4)
  print (id 4)

main2 :: IO ()
main2 = do
  print (roundTrip2 4.0 :: Double)
  print (id 4.0 :: Double)
