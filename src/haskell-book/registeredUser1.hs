{-# OPTIONS -Wall #-}

module RegisteredUser where

newtype Username =
  Username String
  deriving Show

newtype AccountNumber =
  AccountNumber Integer
  deriving Show

data User =
    UnregisteredUser
  | RegisteredUser Username AccountNumber
    deriving Show
