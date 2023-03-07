module TableSalt (TableSalt, parse, encode, unwrap) where

import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import Salt (Salt)
import qualified Salt

newtype TableSalt
  = TableSalt Salt
  deriving (Show)

unwrap :: TableSalt -> Salt
unwrap (TableSalt salt) =
  salt

parse :: Parser TableSalt
parse =
  fmap TableSalt Salt.parse

encode :: TableSalt -> ByteString
encode (TableSalt salt) =
  Salt.encode salt
