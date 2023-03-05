module RawBody (ok, RawBody (..), get, userError, serverError) where

import Data.ByteString.Lazy (ByteString, singleton, unpack)
import Prelude (Eq, Ord, Show, mconcat, show, ($))

newtype RawBody
  = RawBody ByteString
  deriving (Eq, Ord)

instance Show RawBody where
  show (RawBody bytes) =
    mconcat
      [ "RawBody (Data.ByteString.Lazy.pack ",
        show $ unpack bytes,
        ")"
      ]

userError :: RawBody
userError =
  RawBody (singleton 0)

serverError :: RawBody
serverError =
  RawBody (singleton 1)

ok :: RawBody
ok =
  RawBody (singleton 2)

get :: RawBody -> ByteString
get (RawBody raw) =
  raw
