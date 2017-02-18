{-# LANGUAGE NoMonomorphismRestriction #-}

module Ch5 where

-- 5.4

-- 1.a)
-- c)

-- 1.b)
-- d)

-- 1.c)
-- b)

-- 1.d)
-- a)

-- 1.e)
-- e)

-- page 134

-- 1 a
-- 2 d
-- 3 d
-- 4 c
-- 5 a
-- 6 e
-- 7 d
-- 8 a
-- 9 c

-- page 140

ex2a :: a -> a -> a
ex2a x _ = x

ex2b :: a -> a -> a
ex2b _ y = y

ex3 :: a -> b -> b
ex3 _ y = y

f :: Num a => a -> a -> a
f x y = x + y + 3

myConcat :: String -> String
myConcat x = x `mappend` " yo"

myMult :: Fractional a => a -> a
myMult x = (x / 3) * 5

myTake :: Int -> String
myTake x = take x "hey you"

myCom :: Int -> Bool
myCom x = x > length [(1 :: Integer)..10]

myAlph :: Char -> Bool
myAlph x = x < 'z'

-- page 148

-- 1 c
-- 2 a
-- 3 b
-- 4 c

example ::  Num a => a
example = 1

-- page 149


q1a :: Num a => a
q1a = (* 9) 6
-- 1a) 54 :: Num a => a
q1b :: Num t => (t, [Char])
q1b = head [(0,"doge"),(1,"kitteh")]
-- 1b) (0,"doge") :: Num t => (Num t, [Char])
q1c :: (Integer, [Char])
q1c = head [(0 :: Integer,"doge"),(1,"kitteh")]
-- 1c) (0,"doge") :: (Integer, [Char])
--q1d = if False then True else False
-- 1d) False :: Bool
q1e :: Int
q1e = length [(1 :: Integer),2,3,4,5]
-- 1e) 5 :: Int
q1f :: Bool
q1f = length [(1 :: Integer),2,3,4] > length "TACOCAT"
-- 1f) False :: Bool

q2x :: Num t => t
q2x = 5
q2y :: Num a => a
q2y = q2x + 5
q2w :: Num a => a
q2w = q2y * 10
-- q2w :: Num a => a

z :: Num a => a -> a
z _ = q2y * 10
-- z :: Num a => a -> a

q4f :: Fractional a => a
q4f = 4 / q2y
-- q4f :: Fractional a => a

q5x :: [Char]
q5x = "Julie"
q5y :: [Char]
q5y = " <3 "
q5z :: [Char]
q5z = "Haskell"
q5f :: [Char]
q5f = q5x `mappend` q5y `mappend` q5z
-- q5f :: [Char]

-- bigNum = (^) 5 $ 10
bigNum :: Num a => a
bigNum = (^) 5 (10 :: Integer)
wahoo :: Num a => a
wahoo = bigNum + 10

xx :: Show a => a -> IO ()
xx = print
yy :: IO ()
yy = print "woohoo!"
zz :: IO ()
zz = xx "hello ericky"

aa :: Num a => a -> a -> a
aa = (+)
bb :: Num t => t
bb = 6
cc :: Num a => a
cc = aa bb 10
dd :: Num a => a
dd = aa cc 200

aaa :: Num a => a
aaa = 12 + bbb
bbb :: Num a => a
bbb = 10000 * ccc
ccc :: Num t => t
ccc = 4

--addEm :: Int -> Int -> Int
addEm :: Integral a => a -> a -> a
--addEm :: Fractional a => a -> a -> a
addEm x y =
  x + y

functionH :: [a] -> Maybe a
functionH []    = Nothing
functionH (x:_) = Just x

functionC :: Ord a => a -> a -> Bool
--functionC x y = if x > y then True else False
functionC x y =
  x > y

functionS :: (a, b) -> b
functionS (_, y) = y

myFunc :: (x -> y)
       -> (y -> z)
       -> c
       -> (a, x)
       -> (a, z)
myFunc xToY yToZ _ (a, x) =
  (a, yToZ(xToY x))

i :: a -> a
i x =
  x

c :: a -> b -> a
c x _ =
  x

c'' :: b -> a -> b
c'' x _ =
  x

c' :: a -> b -> b
c' _ y =
  y
