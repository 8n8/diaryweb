module Rows (Rows, parse, insert, encode, empty) where

import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as Parsec
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Row (Row)
import qualified Row
import RowId (RowId)

newtype Rows
  = Rows (Map RowId Row)

encode :: Rows -> ByteString
encode (Rows rows) =
  mconcat $ map (\(rowId, row) -> Row.encode rowId row) $ Map.toList rows

insert :: RowId -> Row -> Rows -> Rows
insert rowId row (Rows rows) =
  Rows (Map.insert rowId row rows)

parse :: Parser Rows
parse =
  do
    rows <- Parsec.many1 Row.parse
    return $ Rows (Map.fromList rows)

empty :: Rows
empty =
  Rows Map.empty
