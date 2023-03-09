module Row
    (Row (..)
    , getTableSecret
    , parse, encodeDb, isInTable, getTableSalt) where

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
  = Row TableId TableSalt TableSecret RowSalt UserData
  deriving (Show)

getTableSecret :: Row -> TableSecret
getTableSecret (Row _ _ tableSecret _ _) =
    tableSecret

getTableSalt :: Row -> TableSalt
getTableSalt (Row _ tableSalt _ _ _) =
    tableSalt

isInTable :: TableId -> Row -> Bool
isInTable wanted (Row got _ _ _ _) =
    wanted == got

encodeDb :: RowId -> RowSecret -> Row -> ByteString
encodeDb rowId rowSecret (Row tableId tableSalt tableSecret rowSalt userData) =
  mconcat
    [ TableId.encode tableId,
      TableSalt.encode tableSalt,
      TableSecret.encode tableSecret,
      RowId.encode rowId,
      RowSalt.encode rowSalt,
      RowSecret.encode rowSecret,
      UserData.encodeDb userData
    ]

encodeHttp :: Key -> RowId -> RowSecret -> Row -> ByteString
encodeHttp key rowId _ (Row tableId _ tableSecret _ userData) =
    mconcat
    [ RowId.encode
    , RowSecret.

parse :: Parser ((RowId, RowSecret), Row)
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
      ( (rowId, rowSecret),
        Row tableId tableSalt tableSecret rowSalt userData
      )
