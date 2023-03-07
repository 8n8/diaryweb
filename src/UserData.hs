module UserData (UserData, parseDb, parseHttp, encodeDb) where

import Crypto.Cipher.ChaChaPoly1305 (Nonce)
import qualified Crypto.Cipher.ChaChaPoly1305 as Cipher
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Data.Attoparsec.ByteString (Parser, anyWord8, take, takeByteString)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Key (Key)
import qualified Key
import qualified Nonce
import Prelude
  ( Int,
    Show,
    fail,
    fromIntegral,
    mconcat,
    return,
    ($),
    (*),
    (+),
    (-),
    (<>),
    (>),
  )

newtype UserData
  = UserData ByteString
  deriving (Show)

encodeDb :: UserData -> ByteString
encodeDb (UserData bytes) =
  let length' = ByteString.length bytes
   in ByteString.pack
        [ fromIntegral $ length' .&. 0xff,
          fromIntegral $ (shiftR length' 8) .&. 0xff
        ]
        <> bytes

authenticatedData :: ByteString
authenticatedData =
  "This is the authenticated data for encrypting diaryweb's user data \
  \with ChaChaPoly1305 before storing it in the database."

encrypt :: Key -> Nonce -> ByteString -> CryptoFailable ByteString
encrypt key nonce plaintext =
  do
    initialised <- Cipher.initialize (Key.encode key) nonce
    let (cipherText, afterEncryption) =
          Cipher.encrypt plaintext $
            Cipher.finalizeAAD $
              Cipher.appendAAD authenticatedData initialised

        authTag = Cipher.finalize afterEncryption
    return $
      mconcat
        [ ByteArray.convert nonce,
          ByteArray.convert authTag,
          cipherText
        ]

tagSize :: Int
tagSize =
  16

parseHttp :: Key -> Nonce -> Parser UserData
parseHttp key nonce =
  do
    bytes <- takeByteString
    if ByteString.length bytes > 256 * 256 - Nonce.size - tagSize
      then fail "user data is too long"
      else case encrypt key nonce bytes of
        CryptoFailed _ ->
          fail "couldn't encrypt user data"
        CryptoPassed encrypted ->
          return $ UserData encrypted

parseDb :: Parser UserData
parseDb =
  do
    b0 <- anyWord8
    b1 <- anyWord8
    let length' = fromIntegral b0 + 256 * fromIntegral b1
    bytes <- take length'
    return $ UserData bytes
