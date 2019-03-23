module Monads where

import           Control.Applicative
import           Control.Monad
import           System.Random

x0 = fmap (* 2) (Just 6)
x10 = (* 2) <$> Just 6

x1 = liftA2 (*) (Just 5) (Just 3)
x55 = Just (* 5) <*> Just 3
z5 = Just (+ 5) <*> Just 3

x2 = fmap (* 5) (Just 3)

-- *Monads
z = (+ 3) <$> (+ 2)
-- *Monads
-- λ> z 6
-- 11


half x = if even x then Just (x `div` 2) else Nothing

x3 = fmap half (Just 6)
x4 = fmap half (Just 5)

getNth :: [a] -> Int -> Maybe a
getNth []  i = Nothing
getNth [x] i = Just x
getNth xs  i = Just (xs !! i)

randomNth :: [a] -> IO (Maybe a)
randomNth []  = return Nothing
randomNth [x] = return (Just x)
randomNth xs  = Just . (xs !!) <$> randomRIO (0, length xs - 1)
