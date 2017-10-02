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

printInc :: Integer -> IO ()
printInc n =
  print plusTwo
  where
    plusTwo = n + 2

printInc2 :: Integer -> IO ()
printInc2 n =
  let
    plusTwo = n + 2
    in print plusTwo

-- let x = 5; y = 6 in x * y

mult1 :: Integer
mult1 =
  x * y
  where
    x = 5
    y = 6

plus1 :: Integer
plus1 =
  (x * 3) + y
  where
    x = 3
    y = 1000

plus2 :: Integer
plus2 =
  x * 5
  where
    x = (10 * 5) + y
    y = 10

plus3 :: Double
plus3 =
  z / (x + y)
  where
    x = 7
    y = negate x
    z = y * 10

waxOn :: Int
waxOn =
  x * 5
    where
      z = 7
      y = z + 8
      x = y ^ (2 :: Integer)

triple :: Int -> Int
triple x =
  x * 3

waxOff :: Int -> Int
waxOff =
  triple
