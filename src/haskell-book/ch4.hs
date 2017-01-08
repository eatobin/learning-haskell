{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

module Ch4 where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood    _ = Blah
