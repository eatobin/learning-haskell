module MyTypes where

myAbs :: Int -> Int
myAbs x = abs x

myAbs' :: (a -> a) -> a -> a
myAbs' f x = f x
-- myAbs' abs (-77) -> 77

myConvert :: (a -> b) -> a -> b
myConvert f x = f x
-- myConvert fromIntegral 7 :: Double -> 7.0
