module TableId (TableId, parse, encode) where

import Data.Attoparsec.ByteString (Parser)
import qualified Data.ByteString as Bytes
import Id (Id)
import qualified Id
import Prelude (Show, fmap)

newtype TableId
  = TableId Id
  deriving (Show)

parse :: Parser TableId
parse =
  fmap TableId Id.parse

encode :: TableId -> Bytes.ByteString
encode (TableId id) =
  Id.encode id
