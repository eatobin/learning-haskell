{-# OPTIONS -Wall #-}

module Ch3 where

myString :: String
myString = "Curry is awesome"

curryIsA :: String -> String
curryIsA s = s ++ "!"

curryIsB :: String -> Char
curryIsB s = s !! 4

curryIsC :: String -> String
curryIsC s = drop 9 s ++ "!"

thirdLetter :: String -> Char
thirdLetter x = x !! 3

letterIndex :: Int -> Char
letterIndex x = myString !! x

rvrs :: String -> String
rvrs str =
  drop 9 str ++ " " ++ take 2 (drop 6 str) ++ " " ++ take 5 str

printSecond :: IO ()
printSecond =
  putStrLn greeting
  where
    greeting = "Yarrrrr"

main :: IO ()
main = do
  putStrLn greeting
  printSecond
  where
    greeting = "Yarrrrr"
