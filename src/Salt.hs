module Salt (Salt, parse, encode) where

import Data.Attoparsec.ByteString (Parser, take)
import Data.ByteString (ByteString)
import Prelude (Int, fmap)

newtype Salt
  = Salt ByteString

encode :: Salt -> ByteString
encode (Salt salt) =
  salt

size :: Int
size =
  16

parse :: Parser Salt
parse =
  fmap Salt (take size)
