module Key (Key, parse, encode) where

import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as Parsec
import Data.ByteString (ByteString)

newtype Key
  = Key ByteString

size :: Int
size =
  32

parse :: Parser Key
parse =
  do
    key <- Parsec.take size
    return $ Key key

encode :: Key -> ByteString
encode (Key key) =
  key
