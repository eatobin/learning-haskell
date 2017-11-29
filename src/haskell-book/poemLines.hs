{-# OPTIONS -Wall #-}

module PoemLines where

firstSen :: String
firstSen = "Tyger Tyger, burning bright\n"

secondSen :: String
secondSen = "In the forests of the night\n"

thirdSen :: String
thirdSen = "What immortal hand or eye\n"

fourthSen :: String
fourthSen = "Could frame thy fearful\ \ symmetry?"

sentences :: String
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

-- Implement this
myLines :: String -> [String]
myLines = undefined

-- What we want 'myLines sentences' -- to equal
shouldEqual :: [String]
shouldEqual = [ "Tyger Tyger, burning bright"
              , "In the forests of the night"
              , "What immortal hand or eye"
              , "Could frame thy fearful symmetry?"
              ]

-- The main function here is a small test -- to ensure you've written your function -- correctly.
main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences == shouldEqual)
