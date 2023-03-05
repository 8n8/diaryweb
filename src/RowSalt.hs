module RowSalt (RowSalt, parse, encode, unwrap) where

import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import Salt (Salt)
import qualified Salt

newtype RowSalt
  = RowSalt Salt

unwrap :: RowSalt -> Salt
unwrap (RowSalt salt) =
  salt

parse :: Parser RowSalt
parse =
  fmap RowSalt Salt.parse

encode :: RowSalt -> ByteString
encode (RowSalt salt) =
  Salt.encode salt
