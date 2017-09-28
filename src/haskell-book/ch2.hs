{-# OPTIONS -Wall #-}

module Ch2 where

sayHello :: String -> IO ()
sayHello x =
  putStrLn ("Hello, " ++ x ++ "!")

etTriple :: Integer -> Integer
etTriple x =
  x * 3

pie :: Floating a => a -> a
pie x =
  (x * x) * 3.14

pie2 :: Floating a => a -> a
pie2 x =
  (x * x) * pi

xx :: Integer
xx = 10 * 5 + yy

myResult :: Integer
myResult = xx * 5

yy :: Integer
yy = 10
