module RawStatus (RawStatus, set, get) where

newtype RawStatus
  = RawStatus Int

set :: Int -> RawStatus
set raw =
  RawStatus raw

get :: RawStatus -> Int
get (RawStatus raw) =
  raw
