module RandomBytes (RandomBytes (..)) where

import Data.ByteString (ByteString)

newtype RandomBytes
  = RandomBytes ByteString
