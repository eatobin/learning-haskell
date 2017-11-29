{-# OPTIONS -Wall #-}

module Ch9 where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail [_] = Nothing
safeTail (_:xs) = Just xs

-- λ> 1:2:[] ++ 7:[]
-- [1,2,7]
-- λ> 1:2:[] ++ 7:[] ++ 88:[]
-- [1,2,7,88]
-- λ> 1:2:[] ++ 7:[] ++ 88:89:[]
-- [1,2,7,88,89]

-- λ> [1,3..10]
-- [1,3,5,7,9]
-- λ> enumFromThenTo 1 3 10
-- [1,3,5,7,9]

-- page 308

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool True True = [True]
eftBool False True = [False,True]
eftBool _ _ = []

-- λ> eftBool True True
-- [True]
-- λ> eftBool True False
-- [True,False]
-- λ> eftBool False True
-- [False,True]
-- λ> eftBool False False
-- [False]
-- λ> eftBool True False
-- []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT LT = [LT]
eftOrd EQ EQ = [EQ]
eftOrd GT GT = [GT]
eftOrd LT EQ = [LT,EQ]
eftOrd LT GT = [LT,EQ,GT]
eftOrd EQ GT = [EQ,GT]
eftOrd _ _ = []

-- λ> eftOrd LT LT
-- [LT]
-- λ> eftOrd LT GT
-- [LT,EQ,GT]
-- λ> eftOrd GT LT
-- []
eftInt :: Int -> Int -> [Int]
eftInt f c
  | f > c = []
  | f == c = [f]
  | otherwise =
    f : eftInt (succ f) c

-- λ> eftInt 5 5
-- [5]
-- λ> eftInt 5 6
-- [5,6]
-- λ> eftInt 5 16
-- [5,6,7,8,9,10,11,12,13,14,15,16]
-- λ> eftInt 50 16
-- []
-- λ> eftInt 50 53
-- [50,51,52,53]

eftChar :: Char -> Char -> String
eftChar f c
  | f > c = []
  | f == c = [f]
  | otherwise =
    f : eftChar (succ f) c

-- λ> eftChar 'a' 'a'
-- "a"
-- λ> eftChar 'a' 'f'
-- "abcdef"
-- λ> eftChar 'f' 'a'
-- ""

-- page 311

myWords :: String -> [String]
myWords "" = []
myWords s = takeWhile (/= ' ') s : myWords (dropWhile (== ' ') . dropWhile (/= ' ') $ s)

-- λ> myWords "sheryl wants fun"
-- ["sheryl","wants","fun"]
-- λ> myWords "sheryl"
-- ["sheryl"]
-- λ> myWords ""
-- []

-- Mine
myWords' :: String -> Char -> [String]
myWords' "" _ = []
myWords' s c = takeWhile (/= c) s : myWords' (dropWhile (== c) . dropWhile (/= c) $ s) c

-- λ> myWords' "sheryl wants fun" ' '
-- ["sheryl","wants","fun"]

comp1 :: [Integer]
comp1 = [(x :: Integer)^(2 :: Integer) | x <- [1..10], rem x 2 == 0]
