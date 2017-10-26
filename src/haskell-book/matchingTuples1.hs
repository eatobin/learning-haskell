{-# OPTIONS -Wall #-}

module TupleFunctions where

-- These have to be the same type because
-- (+) is a -> a -> a
addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

-- λ> addEmUp2 (7,8)
-- 15

-- addEmUp2 could also be written like so
addEmUp2Alt :: Num a => (a, a) -> a
-- addEmUp2Alt tup = fst tup + snd tup
addEmUp2Alt = uncurry (+)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, z) = z

-- pg 235

k :: (a, b) -> a
k (x, _) = x

k2 :: String
k2 = k ("three", (1 :: Integer) + (2 :: Integer))

-- b) What is the type of k2? Is it the same type as k1 or k3? Nope! (Integer)

k1 :: Integer
k1 = k ((4 :: Integer) - (1 :: Integer), 10 :: Integer)

k3 :: Integer
k3 = k (3, True)

-- c) k1 & k3 = 3

fur :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
fur (a, _, c) (d, _, f) = ((a, d), (c, f))
