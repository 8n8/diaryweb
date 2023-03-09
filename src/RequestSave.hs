module RequestSave
    (RequestSave
    , tableId, tableSecret, rowId, rowSecret, userData, parse) where

import Crypto.Cipher.ChaChaPoly1305 (Nonce)
import Data.Attoparsec.ByteString (Parser, word8)
import Key (Key)
import RowId (RowId)
import qualified RowId
import RowSalt (RowSalt)
import RowSecret (RowSecret)
import qualified RowSecret
import TableId (TableId)
import qualified TableId
import TableSalt (TableSalt)
import TableSecret (TableSecret)
import qualified TableSecret
import UserData (UserData)
import qualified UserData

data RequestSave = RequestSave
  { tableId :: TableId,
    tableSecret :: TableSecret,
    rowId :: RowId,
    rowSecret :: RowSecret,
    userData :: UserData
  }

parse :: Key -> Nonce -> TableSalt -> RowSalt -> Parser RequestSave
parse key nonce tableSalt rowSalt =
  do
    _ <- word8 0
    tableId_ <- TableId.parse
    tableSecret_ <- TableSecret.parseHttp tableSalt
    rowId_ <- RowId.parse
    rowSecret_ <- RowSecret.parseHttp rowSalt
    userData_ <- UserData.parseHttp key nonce
    return $ RequestSave tableId_ tableSecret_ rowId_ rowSecret_ userData_
