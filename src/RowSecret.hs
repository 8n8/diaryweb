module RowSecret (RowSecret, parseHttp, parseDb, encode) where

import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import RowSalt (RowSalt)
import qualified RowSalt
import UserSecret (UserSecret)
import qualified UserSecret

newtype RowSecret
  = RowSecret UserSecret
  deriving (Show)

encode :: RowSecret -> ByteString
encode (RowSecret secret) =
  UserSecret.encode secret

parseDb :: Parser RowSecret
parseDb =
  fmap RowSecret UserSecret.parseDb

parseHttp :: RowSalt -> Parser RowSecret
parseHttp salt =
  fmap RowSecret (UserSecret.parseHttp (RowSalt.unwrap salt))
