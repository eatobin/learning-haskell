module Reverse where

myString1 :: String
myString1 = "Curry is awesome"

rvrs :: String -> String
rvrs x = drop 9 x `mappend` " " `mappend` take 2 (drop 6 x) `mappend` " " `mappend` take 5 x

main :: IO ()
main = print $ rvrs myString1
