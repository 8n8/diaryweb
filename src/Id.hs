module Id (Id, parse, encode) where

import Data.Word (Word8, Word64)
import Data.ByteString (ByteString, pack)
import Data.Bits ((.&.), shiftR, shiftL)
import Data.Attoparsec.ByteString (Parser, anyWord8)
import Prelude
    (Int, (==)
    , (*), map, ($), fromIntegral, (+), return, fmap, take)

newtype Id
    = Id Word64

parse :: Parser Id
parse =
    parseHelp 0 0

parseHelp :: Int -> Word64 -> Parser Id
parseHelp index accumulated =
    if index == 8 then
        return $ Id accumulated

    else
    do
    byte <- fmap fromIntegral anyWord8
    parseHelp
        (index + 1)
        (accumulated + byte `shiftL` (8 * index))

encode :: Id -> ByteString
encode (Id id) =
    pack $
    map (fromIntegral :: Word64 -> Word8) $
    map (.&. 0xFF) $
    map (\shift -> id `shiftR` shift) $
    map (* 8) $
    take 8 [0..]
