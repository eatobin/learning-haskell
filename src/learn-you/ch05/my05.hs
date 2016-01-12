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

z :: [(Integer, Char)]
z = zip [1,2,3,4,5] "hello"
-- [(1,'h'),(2,'e'),(3,'l'),(4,'l'),(5,'o')]

z' :: [(Char, Integer)]
z' = flip zip [1,2,3,4,5] "hello"
-- [('h',1),('e',2),('l',3),('l',4),('o',5)]

largestDivisible :: Integer
largestDivisible = head (filter p [100000,99999..])
  where p x = mod x 3829 == 0
-- 99554

-- takeWhile (/= ' ') "elephants know how to party"

mySum :: Int
mySum = sum (takeWhile (< 10000) (filter odd (map (^2) [1..])))

-- Collatz Chain
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n : chain (div n 2)
  | odd n = n : chain ((n * 3) + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

-- map chain [1..5]
-- ->[[1],[2,1],[3,10,5,16,8,4,2,1],[4,2,1],[5,16,8,4,2,1]]

-- numLongChains -> 66

listOfFuns :: [Integer -> Integer]
listOfFuns = map (*) [0..]
-- (listOfFuns !! 4) 5 -> 20

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

myMap :: (Num a) => [a]
myMap = map (+3) [1,6,3,2]

myMap' :: (Fractional a) => [a]
myMap' = map (\x -> x + 3.0) [1.4,55.55,6.0]

myMap2 :: [Int]
myMap2 = map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

myZipWith :: [[Char]]
myZipWith = zipWith (flip (++)) ["love you", "love me"] ["i ", "you "]
-- ["i love you","you love me"]

myZipWith' :: [[Char]]
myZipWith' = zipWith (++) ["love you", "love me"] ["i ", "you "]
-- ["love youi ","love meyou "]

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum1 :: (Num a) => [a] -> a
sum1 = foldl (\acc x -> acc + x) 0

sum2 :: (Num a) => [a] -> a
sum2 = foldl (\acc x -> acc + x) 100

sum3 :: (Num a) => [a] -> a
sum3 = foldl (+) 100

sum4 :: (Num a) => [a] -> a
sum4 = foldr (+) 400

eric :: [[Char]] -> [[Char]]
eric = foldr (\x acc -> (x ++ " and Eric!") : acc) []
