module My02 where

myAbs :: Num a => a -> a
myAbs = abs

myAbs' :: Num a => (a -> a) -> a -> a
myAbs' f = f
-- myAbs' abs (-77) -> 77

myConvert :: (a -> b) -> a -> b
myConvert f = f
-- myConvert fromIntegral 7 :: Double -> 7.0

myLength :: Foldable t => t a -> Int
myLength = length

myLengthAdd :: Float
myLengthAdd = fromIntegral (length [1, 2, 3, 4]) + 3.2

myLengthAdd' :: (Foldable t, Fractional b) => t a -> b
myLengthAdd' xs = fromIntegral (length xs) + 3.2

myHead :: [a] -> a
myHead = head

myOrd :: Ord a => a -> a -> Bool
myOrd a b = a > b

myOrd' :: Ord a => a -> a -> Ordering
myOrd' = compare

myRead = read "5" :: Int

myRead' :: Float
myRead' = read "5"
