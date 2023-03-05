module RawPath (RawPath, get, set) where

import Data.Text (Text)

newtype RawPath
  = RawPath [Text]

get :: RawPath -> [Text]
get (RawPath raw) =
  raw

set :: [Text] -> RawPath
set raw =
  RawPath raw
