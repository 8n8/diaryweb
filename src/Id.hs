module Id (Id, parse, encode) where

import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import Data.Word (Word64)
import qualified U64
import Prelude (Eq, Ord, Show, compare, fmap, (==))

newtype Id
  = Id Word64
  deriving (Show)

instance Eq Id where
  (==) (Id a) (Id b) =
    a == b

instance Ord Id where
  compare (Id a) (Id b) =
    compare a b

parse :: Parser Id
parse =
  fmap Id U64.parse

encode :: Id -> ByteString
encode (Id id) =
  U64.encode id
