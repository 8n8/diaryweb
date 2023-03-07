module Main (main) where

import qualified Crypto.Cipher.ChaChaPoly1305 as Cipher
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import qualified Crypto.KDF.Argon2 as Argon2
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Diary (diary)
import RandomBytes (RandomBytes (RandomBytes))
import RawBody (RawBody (RawBody))
import qualified RawBody
import RawDb (RawDb (RawDb))
import RawKey (RawKey (RawKey))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main =
  defaultMain $ testGroup "Unit tests" $ map oneTest cases

oneTest :: Case -> TestTree
oneTest c =
  testCase (description c) $
    (@?=)
      (diary (randomBytes c) (dbKey c) (bodyIn c) (dbIn c))
      (bodyOut c, dbOut c)

rowId :: ByteString
rowId =
  Strict.pack
    [ 86,
      198,
      92,
      113,
      185,
      98,
      148,
      99
    ]

rowSecret :: ByteString
rowSecret =
  Strict.pack
    [ 169,
      149,
      59,
      132,
      3,
      79,
      205,
      228
    ]

tableId :: ByteString
tableId =
  Strict.pack
    [80, 233, 15, 68, 70, 164, 245, 25]

tableSecret :: ByteString
tableSecret =
  Strict.pack
    [ 187,
      72,
      203,
      92,
      100,
      84,
      93,
      134
    ]

tableSalt :: ByteString
tableSalt =
  Strict.pack
    [57, 63, 34, 178, 55, 125, 165, 141, 66, 11, 229, 185, 232, 0, 188, 110]

rowSalt :: ByteString
rowSalt =
  Strict.pack
    [54, 90, 128, 1, 64, 145, 208, 244, 93, 180, 117, 142, 248, 241, 51, 236]

rawNonce :: ByteString
rawNonce =
  Strict.pack
    [184, 126, 62, 202, 15, 184, 127, 101, 136, 220, 85, 144]

databaseKey :: ByteString
databaseKey =
  Strict.pack
    [138, 114, 76, 119, 28, 125, 119, 29, 34, 102, 22, 97, 196, 106, 251, 17, 125, 211, 229, 239, 18, 236, 255, 93, 119, 167, 35, 101, 92, 9, 95, 9]

hashedTableSecret :: ByteString
hashedTableSecret =
  hash tableSecret tableSalt

hashedRowSecret :: Strict.ByteString
hashedRowSecret =
  hash rowSecret rowSalt

hash :: ByteString -> ByteString -> ByteString
hash password salt =
  case Argon2.hash
    ( Argon2.Options
        { Argon2.iterations = 1,
          Argon2.memory = 64 * 1024,
          Argon2.parallelism = 4,
          Argon2.variant = Argon2.Argon2id,
          Argon2.version = Argon2.Version13
        }
    )
    password
    salt
    16 of
    Crypto.Error.CryptoPassed result ->
      result
    Crypto.Error.CryptoFailed _ ->
      error "hashing the secret failed in test"

authenticatedData :: ByteString
authenticatedData =
  "This is the authenticated data for encrypting diaryweb's user data \
  \with ChaChaPoly1305 before storing it in the database."

encrypt ::
  ByteString ->
  ByteString ->
  ByteString ->
  CryptoFailable ByteString
encrypt plaintext nonceBytes key =
  do
    nonce <- Cipher.nonce12 nonceBytes
    state0 <- Cipher.initialize key nonce
    let state1 = Cipher.appendAAD authenticatedData state0
        state2 = Cipher.finalizeAAD state1
        (ciphertext, state3) = Cipher.encrypt plaintext state2
        authTag = Cipher.finalize state3
    return $ nonceBytes <> ByteArray.convert authTag <> ciphertext

-- This will be 2 + 12 + 16 = 30 bytes.
encryptedUserData :: ByteString
encryptedUserData =
  case encrypt "Hi" rawNonce databaseKey of
    Crypto.Error.CryptoFailed _ ->
      error "failed to encrypt user data in test"
    Crypto.Error.CryptoPassed result ->
      result

data Case = Case
  { description :: String,
    bodyIn :: RawBody,
    dbIn :: RawDb,
    dbKey :: RawKey,
    randomBytes :: RandomBytes,
    bodyOut :: RawBody,
    dbOut :: RawDb
  }

saveIndicator :: ByteString
saveIndicator =
  Strict.singleton 0

cases :: [Case]
cases =
  [ Case
      { description = "store a new entry item in the database",
        bodyIn =
          RawBody $
            Lazy.fromStrict $
              mconcat
                [ saveIndicator,
                  tableId <> tableSecret,
                  rowId <> rowSecret,
                  "Hi"
                ],
        dbIn = RawDb "",
        dbKey = RawKey databaseKey,
        randomBytes = RandomBytes (rowSalt <> tableSalt <> rawNonce),
        bodyOut = RawBody.ok,
        dbOut =
          RawDb $
            mconcat
              [ Strict.pack [0, 0, 0, 0, 0, 0, 0, 0], -- next available unique
                tableId,
                tableSalt,
                hashedTableSecret,
                rowId,
                rowSalt,
                hashedRowSecret,
                Strict.pack [30, 0], -- size of encrypted user data
                encryptedUserData
              ]
      }
  ]
