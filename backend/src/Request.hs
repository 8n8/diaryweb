module Request (Request (..), parse) where

import Capacity (Capacity)
import qualified Capacity
import Data.Attoparsec.ByteString (Parser, choice, endOfInput, word8)
import qualified Indicator
import Row (Row)
import qualified Row

data Request
  = Create Row
  | Get Capacity
  | Delete Capacity

instance FromJSON Request where
    parseJSON (Object v) =

