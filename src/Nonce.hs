module Nonce (parse, encode, size) where

import Crypto.Cipher.ChaChaPoly1305 (Nonce)
import qualified Crypto.Cipher.ChaChaPoly1305 as Cipher
import qualified Crypto.Error
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as Parsec
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)

parse :: Parser Nonce
parse =
  do
    raw <- Parsec.take size
    case Cipher.nonce12 raw of
      Crypto.Error.CryptoFailed _ ->
        fail "could not construct nonce"
      Crypto.Error.CryptoPassed nonce ->
        return nonce

encode :: Nonce -> ByteString
encode nonce =
  ByteArray.convert nonce

size :: Int
size =
  12
