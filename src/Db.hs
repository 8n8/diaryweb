module Db (Db, parse, insertRow, encode, getTable) where

import Data.Attoparsec.ByteString (Parser, choice, endOfInput)
import Data.ByteString (ByteString)
import Row (Row (Row))
import RowId (RowId)
import RowSalt (RowSalt)
import RowSecret (RowSecret)
import Rows (Rows)
import qualified Rows
import TableId (TableId)
import TableSalt (TableSalt)
import TableSecret (TableSecret)
import Unique (Unique)
import qualified Unique
import UserData (UserData)

data Db
  = Db Unique Rows

getTable :: TableId -> Db -> [(RowId, RowSecret, Row)]
getTable tableId (Db _ rows) =
    Rows.getTable tableId rows

encode :: Db -> ByteString
encode (Db unique rows) =
  Unique.encode unique <> Rows.encode rows

insertRow ::
  Db ->
  RowId ->
  TableId ->
  TableSalt ->
  TableSecret ->
  RowSalt ->
  RowSecret ->
  UserData ->
  Db
insertRow
  (Db unique rows)
  rowId
  tableId
  tableSalt
  tableSecret
  rowSalt
  rowSecret
  userData =
    Db
      unique
      ( Rows.insert
          rowId
          rowSecret
          (Row tableId tableSalt tableSecret rowSalt userData)
          rows
      )

parse :: Parser Db
parse =
  choice
    [ do
        _ <- endOfInput
        return $ Db Unique.first Rows.empty,
      parseNonEmpty
    ]

parseNonEmpty :: Parser Db
parseNonEmpty =
  do
    unique <- Unique.parse
    rows <- Rows.parse
    _ <- endOfInput
    return $ Db unique rows
