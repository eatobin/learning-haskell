{-# LANGUAGE OverloadedStrings #-}

module MyAeson where

import Data.Aeson
import Data.Text
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy.IO as I
import GHC.Exts

-- val :: Value
-- val = Object $ fromList [
--   ("numbers", Array $ fromList [Number 1, Number 2, Number 3]),
--   ("boolean", Bool True) ]

val :: Value
val =
  object
    [ "boolean" .= True,
      "numbers" .= [1, 2, 3 :: Int]
    ]

y0 = decode "[1,2,3]" :: Maybe [Integer]

y1 = I.putStrLn . E.decodeUtf8 . encode $ val
