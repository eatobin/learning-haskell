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
myCom x = x > (length [1..10])

myAlph :: Char -> Bool
myAlph x = x < 'z'
