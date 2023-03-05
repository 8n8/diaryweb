module RawKey (RawKey (..), unwrap) where

import Data.ByteString (ByteString)

newtype RawKey
  = RawKey ByteString

unwrap :: RawKey -> ByteString
unwrap (RawKey raw) =
  raw
