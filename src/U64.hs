module U64 (parse, encode) where

import Data.Attoparsec.ByteString (Parser, anyWord8)
import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString (ByteString, pack)
import Data.Word (Word64, Word8)

parse :: Parser Word64
parse =
  parseHelp 0 0

parseHelp :: Int -> Word64 -> Parser Word64
parseHelp index accumulated =
  if index == 8
    then return accumulated
    else do
      byte <- fmap fromIntegral anyWord8
      parseHelp
        (index + 1)
        (accumulated + byte `shiftL` (8 * index))

encode :: Word64 -> ByteString
encode word =
  pack $
    map (fromIntegral :: Word64 -> Word8) $
      map (.&. 0xFF) $
        map (\shift -> word `shiftR` shift) $
          map (* 8) $
            take 8 [0 ..]
