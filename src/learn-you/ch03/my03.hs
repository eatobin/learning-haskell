module My03 where

myAdd :: Int
myAdd = a + b
  where a = 5
        b = 6

myAdd' :: Int -> Int -> Int
myAdd' a b = x + y
  where x = a * b
        y = div a b

myAdd'' :: Int -> Int -> Int
myAdd'' a b =
  let x = a * b
      y = div a b
  in x + y

myAddSmall :: Int -> Int
myAddSmall a
  | a <= 100  = 0
  | otherwise = a

myAddSmall' :: Int -> Int -> Int
myAddSmall' a b
  | b == 0    = error "Division by 0!"
  | otherwise = x + y
  where x = a * b
        y = div a b

myAbs :: Int -> Int
myAbs n | n >= 0    = n
        | otherwise = -n

mySignum :: Int -> Int
mySignum n
  | n <  0    = -1
  | n == 0    =  0
  | otherwise =  1

myAnd :: Bool -> Bool -> Bool
myAnd True True = True
myAnd _    _    = False
-- myAnd (1 == 1) (2 == 3) -> False

myAnd' :: Bool -> Bool -> Bool
myAnd' True  b = b
myAnd' False _ = False
-- myAnd' (1 == 1) (2 == 3) -> False

memory :: [a] -> String
memory []     = "Empty"
memory [a]    = "One"
memory (a:as) = "Many"
