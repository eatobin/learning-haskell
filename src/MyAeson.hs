{-# LANGUAGE OverloadedStrings #-}

module MyAeson where

import           Data.Aeson
import           Data.Text
import           GHC.Exts

val :: Value
val = Object $ fromList [
  ("numbers", Array $ fromList [Number 1, Number 2, Number 3]),
  ("boolean", Bool True) ]
