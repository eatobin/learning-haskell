module Ch3 where

waxOn :: Int
waxOn =
  x * 5
    where
      z = 7
      y = z + 8
      x = y ^ (2 :: Integer)

triple :: Int -> Int
triple x =
  x * 3

waxOff :: Int -> Int
waxOff =
  triple

myString :: String
myString = "Curry is awesome"

curryIsA :: String -> String
curryIsA s = s `mappend` "!"

curryIsB :: String -> Char
curryIsB s = s !! 4

curryIsC :: String -> String
curryIsC s = drop 9 s `mappend` "!"

thirdLetter :: String -> Char
thirdLetter x = x !! 3

letterIndex :: Int -> Char
letterIndex x = myString !! x

rvrs :: String
rvrs =
  drop 9 myString `mappend` " " `mappend` take 2 (drop 6 myString) `mappend` " " `mappend` take 5 myString
