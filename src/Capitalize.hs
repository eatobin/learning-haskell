{-# OPTIONS -Wall #-}

module Capitalize where

import           Data.Char

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : capitalize xs

capitalize' :: IO [Char]
capitalize' = map toUpper <$> getLine
