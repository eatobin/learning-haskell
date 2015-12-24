module My04 where

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)
-- maximum [2,5,1]
-- max 2 (maximum' [5,1])
-- max 5 (maximum' [1])
-- max 5 1
-- max 2 5

-- from Seven Langs
backwards :: (Num a) => [a] -> [a]
backwards [] = []
backwards (h : t) = backwards t ++ [h]
-- backwards [1,2,3] ++ [1]
-- backwards [2,3] ++ [2]
-- backwards [3] ++ [3]
-- backwards [] ++ []
-- [] ++ [3] ++ [2] ++ [1]
-- [3,2,1]

backwards' :: (Num a) => [a] -> [a]
backwards' [] = [99]
backwards' (h : t) = backwards' t ++ [h]
-- backwards [1,2,3] ++ [1]
-- backwards [2,3] ++ [2]
-- backwards [3] ++ [3]
-- backwards [] ++ [99]
-- [99] ++ [3] ++ [2] ++ [1]
-- [99,3,2,1]

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x
-- let take4 = take 4
-- (take4 . repeat') 5
-- [5,5,5,5]

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x : xs)
  | a == x = True
  | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerOrEqual = [a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
  in quicksort smallerOrEqual ++ [x] ++ quicksort larger
