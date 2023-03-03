module UserData (Http, Db, parseDb, parseHttp, encodeHttp, encodeDb) where

import qualified Data.ByteString as Bytes
import qualified Data.Attoparsec.ByteString as Parsec
import qualified Data.Bits as Bits
import Prelude (return, fromIntegral, (*), (<>), ($), (+), fail, (>))

newtype Http
    = Http Bytes.ByteString

newtype Db
    = Db Bytes.ByteString

encodeHttp :: Http -> Bytes.ByteString
encodeHttp (Http bytes) =
    encode bytes

encodeDb :: Db -> Bytes.ByteString
encodeDb (Db bytes) =
    encode bytes

encode :: Bytes.ByteString -> Bytes.ByteString
encode bytes =
    let
        length = Bytes.length bytes
    in
    Bytes.pack
    [ fromIntegral $ length Bits..&. 0xff
    , fromIntegral $ (Bits.shiftR length 8) Bits..&. 0xff
    ]
        <> bytes

parseHttp :: Parsec.Parser Http
parseHttp =
    do
    bytes <- Parsec.takeByteString
    if Bytes.length bytes > 2 `Bits.shiftL` 16 then
        fail "user data from HTTP is too long"
    else
        return $ Http bytes

parseDb :: Parsec.Parser Db
parseDb =
    do
    b0 <- Parsec.anyWord8
    b1 <- Parsec.anyWord8
    let length = fromIntegral b0 + 256 * fromIntegral b1
    bytes <- Parsec.take length
    return (Db bytes)
