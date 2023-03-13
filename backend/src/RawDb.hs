module RawDb (RawDb (RawDb), get, set) where

import Data.ByteString (ByteString, unpack)

newtype RawDb
  = RawDb ByteString
  deriving (Eq, Ord)

instance Show RawDb where
  show (RawDb bytes) =
    mconcat
      [ "RawDb (Data.ByteString.pack ",
        show $ unpack bytes,
        ")"
      ]

get :: RawDb -> ByteString
get (RawDb raw) =
  raw

set :: ByteString -> RawDb
set raw =
  RawDb raw
