module UserData (UserData, parse, encode) where

import Data.Attoparsec.ByteString (Parser, anyWord8, take)
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Prelude
  ( Eq,
    Ord,
    Show,
    fromIntegral,
    return,
    ($),
    (*),
    (+),
    (<>),
  )

newtype UserData
  = UserData ByteString
  deriving (Show, Eq, Ord)

encode :: UserData -> ByteString
encode (UserData bytes) =
  let length' = ByteString.length bytes
   in ByteString.pack
        [ fromIntegral $ length' .&. 0xff,
          fromIntegral $ (shiftR length' 8) .&. 0xff
        ]
        <> bytes

parse :: Parser UserData
parse =
  do
    b0 <- anyWord8
    b1 <- anyWord8
    let length' = fromIntegral b0 + 256 * fromIntegral b1
    bytes <- take length'
    return $ UserData bytes
