module UserSecret (Http, parseHttp, parseDb, Db) where

import Data.ByteString (ByteString)
import Salt (Salt, encode)
import Crypto.Error (CryptoFailable(..))
import Crypto.KDF.Argon2
    (version, variant, parallelism, memory, iterations, Options(..)
    , hash, Version(..), Variant(..)
    )
import Data.Attoparsec.ByteString (Parser, take)
import Prelude (Int, fail, ($), return, (*))

newtype Http
  = Http ByteString

rawSize :: Int
rawSize =
  8

hashSize :: Int
hashSize =
  32

options :: Options
options =
  Options
    { iterations = 1
    , memory = 64*1024
    , parallelism = 4
    , variant = Argon2id
    , version = Version13
    }

parseHttp :: Salt -> Parser Http
parseHttp salt =
  do
  bytes <- take rawSize
  case hash options bytes (Salt.encode salt) hashSize of
    CryptoPassed hashed ->
      return $ Http hashed

    CryptoFailed _ ->
      fail "hashing the ID failed"

parseDb :: Parser Db
parseDb =
  do
  bytes <- take hashSize
  return $ Db bytes
