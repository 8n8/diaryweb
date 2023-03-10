module RawBody
  ( fetchedTable,
    userError,
    serverError,
    RawBody (..),
    get,
  )
where

import Data.ByteString.Lazy (ByteString, singleton, unpack)
import qualified Indicator
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

serverError :: RawBody
serverError =
  RawBody (singleton Indicator.serverError)

userError :: RawBody
userError =
  RawBody (singleton Indicator.userError)

get :: RawBody -> ByteString
get (RawBody raw) =
  raw

fetchedTable :: ByteString
fetchedTable =
  singleton 2
