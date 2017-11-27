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
