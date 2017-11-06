{-# OPTIONS -Wall #-}

module Ch8 where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n =
  n * factorial (n - 1)

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

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times = applyTimes times (+1)

applyTimes' :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes' 0 _ b = b
applyTimes' n f b = f . applyTimes' (n-1) f $ b

-- λ> applyTimes' 6 (+2) 5
-- 17
