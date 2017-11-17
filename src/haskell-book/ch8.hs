{-# OPTIONS -Wall #-}

module Ch8 where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n =
  n * factorial (n - 1)

-- λ> factorial 6
-- 720

inc :: Num a => a -> a
inc = (+1)

three :: Integer
three =
  inc . inc . inc $ 6

-- λ> three
-- 9

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 start = start
incTimes times start =
  inc (incTimes (times - 1) start)

-- λ> incTimes 3 6
-- 9
-- λ> incTimes 3 16
-- 19
-- λ> incTimes 3 1
-- 4
-- λ> incTimes 1 3
-- 4

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 _ b = b
applyTimes n f b = f (applyTimes (n-1) f b)

-- λ> applyTimes 0 (+1) 8
-- 8
-- λ> applyTimes 4 (+1) 8
-- 12

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times = applyTimes times (+1)

applyTimes' :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes' 0 _ b = b
applyTimes' n f b = f . applyTimes' (n-1) f $ b

-- λ> applyTimes' 6 (+2) 5
-- 17

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x =
  fibonacci (x - 1) + fibonacci (x - 2)

-- λ> fibonacci 6
-- 8
-- λ> fibonacci 22
-- 17711


-- Scala:
-- def fib(n: Int): Int = {
--   @annotation.tailrec
--   def go(a: Int, b: Int, ctr: Int): Int = {
--     if (ctr == n) a + b
--     else go(b, a + b, ctr + 1)
--   }
--   go(0, 1, 3)
-- }

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom =
  go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise =
          go (n - d) d (count + 1)

-- λ> dividedBy 20 4
-- (5,0)
-- λ> dividedBy 21 4
-- (5,1)

-- page 294

-- 1 d
-- 2 b
-- 3 d
-- 4 b

-- page 295

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- λ> cattyConny "one" "two"
-- "one mrow two"

flippy :: String -> String -> String
flippy = flip cattyConny

-- λ> flippy "one" "two"
-- "two mrow one"

appedCatty :: String -> String
appedCatty = cattyConny "whoops"

-- λ> appedCatty "two"
-- "whoops mrow two"

frappe :: String -> String
frappe = flippy "haha"

-- λ> frappe "one"
-- "one mrow haha"
