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
