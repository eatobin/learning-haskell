module MyTypes where

myAbs ::  Num a => a -> a
myAbs x = abs x

myAbs' :: Num a => (a -> a) -> a -> a
myAbs' f x = f x
-- myAbs' abs (-77) -> 77

myConvert :: (a -> b) -> a -> b
myConvert f x = f x
-- myConvert fromIntegral 7 :: Double -> 7.0

myLength :: Foldable t => t a -> Int
myLength xs = length xs

myLengthAdd :: Float
myLengthAdd = fromIntegral (length [1,2,3,4]) + 3.2

myLengthAdd' :: (Foldable t, Fractional b) => t a -> b
myLengthAdd' xs = fromIntegral (length xs) + 3.2

myHead :: [a] -> a
myHead xs = head xs

myOrd :: Ord a => a -> a -> Bool
myOrd a b = a > b

myOrd' :: Ord a => a -> a -> Ordering
myOrd' a b = compare a b

myRead = read "5" :: Int

myRead' :: Float
myRead' = read "5"
