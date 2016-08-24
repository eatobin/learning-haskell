module Monads where

import Control.Applicative
import Control.Monad

x0 = fmap (*2) (Just 6)

x1 = liftA2 (*) (Just 5) (Just 3)
x2 = liftA (*5) (Just 3)

half x = if even x
           then Just (x `div` 2)
           else Nothing

x3 = liftM half (Just 6)
x4 = liftM half (Just 5)
