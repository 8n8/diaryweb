module RowId (RowId, parse, encode) where

import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import Id (Id)
import qualified Id
import Prelude (Eq, Ord, compare, fmap, (==))

newtype RowId
  = RowId Id

instance Eq RowId where
  (==) (RowId a) (RowId b) =
    a == b

instance Ord RowId where
  compare (RowId a) (RowId b) =
    compare a b

encode :: RowId -> ByteString
encode (RowId id) =
  Id.encode id

parse :: Parser RowId
parse =
  fmap RowId Id.parse
