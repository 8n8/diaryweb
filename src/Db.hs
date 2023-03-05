module Db (Db, parse, insertRow, encode) where

import Data.Attoparsec.ByteString (Parser)
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
          (Row tableId tableSalt tableSecret rowSalt rowSecret userData)
          rows
      )

parse :: Parser Db
parse =
  do
    unique <- Unique.parse
    rows <- Rows.parse
    return $ Db unique rows
