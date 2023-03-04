module TableId (TableId, parse, encode) where

import qualified Data.ByteString as Bytes
import Data.Attoparsec.ByteString (Parser)
import qualified Id
import Prelude (fmap)

newtype TableId
    = TableId Id.Id

parse :: Parser TableId
parse =
    fmap TableId Id.parse

encode :: TableId -> Bytes.ByteString
encode (TableId id) =
    Id.encode id
