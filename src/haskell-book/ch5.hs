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
myConcat x = x ++ " yo"

myMult :: Fractional a => a -> a
myMult x = (x / 3) * 5

myTake :: Int -> String
myTake x = take x "hey you"

myCom :: Int -> Bool
myCom x = x > length [1..10]

myAlph :: Char -> Bool
myAlph x = x < 'z'

-- page 148

-- 1 c
-- 2 a
-- 3 b
-- 4 c

example = 1

-- page 149

q1a = (* 9) 6
-- 1a) 54 :: Num a => a
q1b = head [(0,"doge"),(1,"kitteh")]
-- 1b) (0,"doge") :: Num t => (Num t, [Char])
q1c = head [(0 :: Integer,"doge"),(1,"kitteh")]
-- 1c) (0,"doge") :: (Integer, [Char])
--q1d = if False then True else False
-- 1d) False :: Bool
q1e = length [1,2,3,4,5]
-- 1e) 5 :: Int
q1f = length [1,2,3,4] > length "TACOCAT"
-- 1f) False :: Bool

q2x = 5
q2y = q2x + 5
q2w = q2y * 10
-- q2w :: Num a => a

z q2y = q2y * 10
-- z :: Num a => a -> a

q4f = 4 / q2y
-- q4f :: Fractional a => a

q5x = "Julie"
q5y = " <3 "
q5z = "Haskell"
q5f = q5x ++ q5y ++ q5z
-- q5f :: [Char]
