module RowId (Http, Db, parseDb, parseHttp, encodeDb, encodeHttp) where

import qualified Data.ByteString as Bytes
import qualified Data.Attoparsec.ByteString as Parsec

newtype Http
    = Http Bytes.ByteString

encodeHttp :: Http -> Bytes.ByteString
encodeHttp (Http bytes) =
    bytes

newtype Db
    = Db Bytes.ByteString

encodeDb :: Db -> Bytes.ByteString
encodeDb (Db bytes) =
    bytes

size :: Int
size =
    12

parseHttp :: Parsec.Parser Http
parseHttp =
    do
    bytes <- Parsec.take size
    return $ Http bytes

parseDb :: Parsec.Parser Db
parseDb =
    do
    bytes <- Parsec.take size
    return $ Db bytes
