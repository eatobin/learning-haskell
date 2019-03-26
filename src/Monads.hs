-- http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html

-- Conclusion
-- A functor is a data type that implements the Functor typeclass.
-- An applicative is a data type that implements the Applicative typeclass.
-- A monad is a data type that implements the Monad typeclass.
-- A Maybe implements all three, so it is a functor, an applicative, and a monad.
-- What is the difference between the three?



-- functors: you apply a function to a wrapped value using fmap or <$>
-- applicatives: you apply a wrapped function to a wrapped value using <*> or liftA
-- monads: you apply a function that returns a wrapped value, to a wrapped value using >>= or liftM

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

z = (+ 3) <$> (+ 2)
-- λ> z 6
-- 11


half x = if even x then Just (x `div` 2) else Nothing

-- instance Monad Maybe where
--     Nothing >>= func = Nothing
--     Just val >>= func  = func val

noNad = half 66
-- λ> noNad
-- Just 33
x3 = Just 3 >>= half
x4 = Just 4 >>= half
x5 = Nothing >>= half
x77 = liftM half (Just 34)

getNth :: [a] -> Int -> Maybe a
getNth []  i = Nothing
getNth [x] i = Just x
getNth xs  i = Just (xs !! i)

randomNth :: [a] -> IO (Maybe a)
randomNth []  = return Nothing
randomNth [x] = return (Just x)
randomNth xs  = Just . (xs !!) <$> randomRIO (0, length xs - 1)

-- *Monads
f = readFile "ghcid.txt" >>= putStrLn
-- f
-- ghcid --command="stack repl"

-- *Monads
