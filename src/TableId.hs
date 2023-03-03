module TableId (Http, Db, parseDb, parseHttp, encodeDb, encodeHttp, Salt, makeSalt) where

import qualified Data.ByteString as Bytes
import qualified Data.Attoparsec.ByteString as Parsec
import qualified Crypto.Random as Random

newtype Http
    = Http Bytes.ByteString

newtype Salt
    = Salt Bytes.ByteString

makeSalt :: IO Salt
makeSalt =
  do
  bytes <- Random.getRandomBytes
  return $ Salt bytes

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
