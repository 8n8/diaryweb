module Unique (Unique, parse, encode, first) where

import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import Data.Word (Word64)
import qualified U64

newtype Unique
  = Unique Word64

parse :: Parser Unique
parse =
  fmap Unique U64.parse

encode :: Unique -> ByteString
encode (Unique raw) =
  U64.encode raw

first :: Unique
first =
  Unique 0
