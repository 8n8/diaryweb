module RawTableSecret (RawTableSecret, parse, unwrap) where

import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString (take, Parser)
import Prelude (fmap)

newtype RawTableSecret
    = RawTableSecret ByteString


parse :: Parser RawTableSecret
parse =
    fmap RawTableSecret (take 8)


unwrap :: RawTableSecret -> ByteString
unwrap (RawTableSecret rawTableSecret) =
    rawTableSecret
