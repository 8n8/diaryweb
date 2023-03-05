module Row (Row (..), parse, encode) where

import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import RowId (RowId)
import qualified RowId
import RowSalt (RowSalt)
import qualified RowSalt
import RowSecret (RowSecret)
import qualified RowSecret
import TableId (TableId)
import qualified TableId
import TableSalt (TableSalt)
import qualified TableSalt
import TableSecret (TableSecret)
import qualified TableSecret
import UserData (UserData)
import qualified UserData

data Row
  = Row TableId TableSalt TableSecret RowSalt RowSecret UserData

encode :: RowId -> Row -> ByteString
encode rowId (Row tableId tableSalt tableSecret rowSalt rowSecret userData) =
  mconcat
    [ TableId.encode tableId,
      TableSalt.encode tableSalt,
      TableSecret.encode tableSecret,
      RowId.encode rowId,
      RowSalt.encode rowSalt,
      RowSecret.encode rowSecret,
      UserData.encodeDb userData
    ]

parse :: Parser (RowId, Row)
parse =
  do
    tableId <- TableId.parse
    tableSalt <- TableSalt.parse
    tableSecret <- TableSecret.parseDb
    rowId <- RowId.parse
    rowSalt <- RowSalt.parse
    rowSecret <- RowSecret.parseDb
    userData <- UserData.parseDb
    return $
      ( rowId,
        Row tableId tableSalt tableSecret rowSalt rowSecret userData
      )
