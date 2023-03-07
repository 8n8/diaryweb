module UserSecret (UserSecret, parseHttp, parseDb, encode) where

import qualified Crypto.Error
import qualified Crypto.KDF.Argon2 as Argon2
import Data.Attoparsec.ByteString (Parser, take)
import Data.ByteString (ByteString)
import Salt (Salt)
import qualified Salt
import Prelude (Int, Show, fail, return, ($), (*))

newtype UserSecret
  = UserSecret ByteString
  deriving (Show)

encode :: UserSecret -> ByteString
encode (UserSecret raw) =
  raw

rawSize :: Int
rawSize =
  8

hashSize :: Int
hashSize =
  16

options :: Argon2.Options
options =
  Argon2.Options
    { Argon2.iterations = 1,
      Argon2.memory = 64 * 1024,
      Argon2.parallelism = 4,
      Argon2.variant = Argon2.Argon2id,
      Argon2.version = Argon2.Version13
    }

parseHttp :: Salt -> Parser UserSecret
parseHttp salt =
  do
    bytes <- take rawSize
    case Argon2.hash options bytes (Salt.encode salt) hashSize of
      Crypto.Error.CryptoPassed hashed ->
        return $ UserSecret hashed
      Crypto.Error.CryptoFailed _ ->
        fail "hashing the ID failed"

parseDb :: Parser UserSecret
parseDb =
  do
    bytes <- take hashSize
    return $ UserSecret bytes
