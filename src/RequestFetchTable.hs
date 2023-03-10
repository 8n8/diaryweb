module RequestFetchTable
  ( RequestFetchTable,
    parse,
    tableId,
    rawTableSecret,
  )
where

import Data.Attoparsec.ByteString (Parser, word8)
import RawTableSecret (RawTableSecret)
import qualified RawTableSecret
import TableId (TableId)
import qualified TableId

data RequestFetchTable = RequestFetchTable
  { tableId :: TableId,
    rawTableSecret :: RawTableSecret
  }

parse :: Parser RequestFetchTable
parse =
  do
    _ <- word8 1
    tableId_ <- TableId.parse
    tableSecret_ <- RawTableSecret.parse
    return $ RequestFetchTable tableId_ tableSecret_
