module Id (Http, parseHttp, parseDb) where

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
  12

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

salt :: ByteString
salt =
    pack [ 12, 241, 34, 209, 144, 189, 14, 8, 148, 146, 238, 124, 116, 62, 174, 93, 228, 250, 157, 37, 95, 97, 203, 130, 85, 31, 12, 6, 112, 107, 39, 247 ]

parseHttp :: Parser Http
parseHttp =
  do
  bytes <- take rawSize
  case hash options bytes salt hashSize of
    CryptoPassed hashed ->
      return $ Http hashed

    CryptoFailed _ ->
      fail "hashing the ID failed"

parseDb :: Parser Db
parseDb =
  do
  bytes <- take hashSize
  return $ Db bytes
