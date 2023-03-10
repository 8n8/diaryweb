module RawTableSecret (RawTableSecret, parse, unwrap) where

import Data.Attoparsec.ByteString (Parser, take)
import Data.ByteString (ByteString)
import Prelude (fmap)

newtype RawTableSecret
  = RawTableSecret ByteString

parse :: Parser RawTableSecret
parse =
  fmap RawTableSecret (take 8)

unwrap :: RawTableSecret -> ByteString
unwrap (RawTableSecret rawTableSecret) =
  rawTableSecret
