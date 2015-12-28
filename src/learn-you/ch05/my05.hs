module My05 where

multiThree :: Int -> Int -> Int -> Int
multiThree x y z = x * y * z

compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: Int -> Ordering
compareWithHundred' = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum a = elem a ['A'..'Z']
