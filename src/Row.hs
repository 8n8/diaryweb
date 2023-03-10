module Row (Row (..), parse, encode, capacity) where

import Capacity (Capacity)
import qualified Capacity
import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import UserData (UserData)
import qualified UserData

data Row
  = Row Capacity UserData
  deriving (Show, Eq, Ord)

encode :: Row -> ByteString
encode (Row capacity_ userData) =
  Capacity.encode capacity_ <> UserData.encode userData

capacity :: Row -> Capacity
capacity (Row capacity_ _) =
  capacity_

parse :: Parser Row
parse =
  do
    capacity_ <- Capacity.parse
    userData <- UserData.parseDb
    return $ Row capacity_ userData
