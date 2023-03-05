module TableSecret (TableSecret, parseHttp, parseDb, encode) where

import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import TableSalt (TableSalt)
import qualified TableSalt
import UserSecret (UserSecret)
import qualified UserSecret

newtype TableSecret
  = TableSecret UserSecret

encode :: TableSecret -> ByteString
encode (TableSecret secret) =
  UserSecret.encode secret

parseDb :: Parser TableSecret
parseDb =
  fmap TableSecret UserSecret.parseDb

parseHttp :: TableSalt -> Parser TableSecret
parseHttp salt =
  fmap TableSecret (UserSecret.parseHttp (TableSalt.unwrap salt))
