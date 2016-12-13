{-# LANGUAGE OverloadedStrings #-}

module MyAeson where

import           Data.Aeson
import           Data.Text
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO       as T
import           GHC.Exts

val :: Value
val = Object $ fromList [
  ("numbers", Array $ fromList [Number 1, Number 2, Number 3]),
  ("boolean", Bool True) ]

y0 = decode "[1,2,3]" :: Maybe [Integer]
y1 = T.putStrLn . T.decodeUtf8 . encode $ val
