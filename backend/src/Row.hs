module Row (Row (..), parse, encodeDb, capacity, encodeHttp) where

import Capacity (Capacity)
import qualified Capacity
import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import UserData (UserData)
import qualified UserData

data Row
  = Row Capacity UserData
  deriving (Show, Eq, Ord)

encodeDb :: Row -> ByteString
encodeDb (Row capacity_ userData) =
  Capacity.encode capacity_ <> UserData.encode userData

encodeHttp :: Row -> ByteString
encodeHttp (Row _ userData) =
  UserData.encode userData

capacity :: Row -> Capacity
capacity (Row capacity_ _) =
  capacity_

parse :: Parser Row
parse =
  do
    capacity_ <- Capacity.parse
    userData <- UserData.parse
    return $ Row capacity_ userData
