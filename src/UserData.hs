module UserData (UserData, parse, encode) where

import Data.ByteString (ByteString, pack, length)
import Data.Attoparsec.ByteString (anyWord8, take, Parser)
import Data.Bits ((.&.), shiftR)
import Prelude (return, fromIntegral, (*), (<>), ($), (+))

newtype UserData
    = UserData ByteString

encode :: UserData -> ByteString
encode (UserData bytes) =
    let
        length' = length bytes
    in
    pack
    [ fromIntegral $ length' .&. 0xff
    , fromIntegral $ (shiftR length' 8) .&. 0xff
    ]
        <> bytes

parse :: Parser UserData
parse =
    do
    b0 <- anyWord8
    b1 <- anyWord8
    let length' = fromIntegral b0 + 256 * fromIntegral b1
    bytes <- take length'
    return (UserData bytes)
