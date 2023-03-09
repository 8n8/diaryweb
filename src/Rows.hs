module Rows (Rows, parse, insert, encode, empty, getTable) where

import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as Parsec
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Row (Row)
import qualified Row
import RowId (RowId)
import RowSecret (RowSecret)
import TableId (TableId)

newtype Rows
  = Rows (Map (RowId, RowSecret) Row)

getTable :: TableId -> Rows -> [(RowId, RowSecret, Row)]
getTable tableId (Rows rows) =
    map (\((rowId, rowSecret), row) -> (rowId, rowSecret, row)) $
    filter (\(_, row) -> Row.isInTable tableId row) $
    Map.toList rows

encode :: Rows -> ByteString
encode (Rows rows) =
  mconcat $
  map (\((rowId, rowSecret), row) -> Row.encodeDb rowId rowSecret row) $
  Map.toList rows

insert :: RowId -> RowSecret -> Row -> Rows -> Rows
insert rowId rowSecret row (Rows rows) =
  Rows (Map.insert (rowId, rowSecret) row rows)

parse :: Parser Rows
parse =
  do
    rows <- Parsec.many1 Row.parse
    return $ Rows (Map.fromList rows)

empty :: Rows
empty =
  Rows Map.empty
