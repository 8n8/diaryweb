module Capacity (Capacity, parse, encode) where

import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as Parsec
import Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)

newtype Capacity
  = Capacity ScrubbedBytes
  deriving (Show, Ord, Eq)

encode :: Capacity -> ByteString
encode (Capacity capacity) =
  ByteArray.convert capacity

parse :: Parser Capacity
parse =
  do
    raw <- Parsec.take 16
    return $ Capacity $ ByteArray.convert raw
