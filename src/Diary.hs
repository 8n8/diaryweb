module Diary (diary) where

import Crypto.Cipher.ChaChaPoly1305 (Nonce)
import Data.Attoparsec.ByteString (Parser, endOfInput, parseOnly)
import qualified Data.ByteString.Lazy as Lazy
import Db (Db)
import qualified Db
import qualified Key
import qualified Nonce
import RandomBytes (RandomBytes (RandomBytes))
import Data.ByteString (ByteString)
import RawBody (RawBody(RawBody))
import qualified RawBody
import TableSecret (TableSecret)
import RawBody (RawBody)
import qualified Row
import Row (Row)
import RowSecret (RowSecret)
import RowId (RowId)
import RawDb (RawDb (RawDb))
import RequestFetchTable (RequestFetchTable)
import qualified RawDb
import qualified RawTableSecret
import RawKey (RawKey)
import qualified RawKey
import Request (Request)
import qualified Request
import RequestSave (RequestSave)
import qualified RequestSave
import RowSalt (RowSalt)
import qualified RowSalt
import TableSalt (TableSalt)
import qualified TableSalt
import qualified RequestFetchTable
import qualified TableSecret

diary :: RandomBytes -> RawKey -> RawBody -> RawDb -> (RawBody, RawDb)
diary randomBytes rawKey rawBody rawDb =
  case parse randomBytes rawKey rawBody rawDb of
    Left err ->
      (err, rawDb)
    Right (request, tableSalt, rowSalt, db) ->
      handleValidRequest request db tableSalt rowSalt

handleValidRequest ::
  Request ->
  Db ->
  TableSalt ->
  RowSalt ->
  (RawBody, RawDb)
handleValidRequest request db tableSalt rowSalt =
  case request of
    Request.Save save ->
      handleSaveRequest save tableSalt rowSalt db

    Request.FetchTable fetchTable ->
      handleFetchTable fetchTable db

handleFetchTable :: RequestFetchTable -> Db -> (RawBody, RawDb)
handleFetchTable request db =
    case Db.getTable (RequestFetchTable.tableId request) db of
        [] ->
            (RawBody.userError, RawDb $ Db.encode db)

        top : remainder ->
            let
                tableSalt :: TableSalt
                tableSalt = 
                    Row.getTableSalt (thd top)

                rawTableSecret :: ByteString
                rawTableSecret =
                    RawTableSecret.unwrap $
                      RequestFetchTable.rawTableSecret request

                tableSecretParser :: Parser TableSecret
                tableSecretParser =
                    TableSecret.parseHttp tableSalt
            in
            case parseOnly tableSecretParser rawTableSecret of
            Left _ ->
                (RawBody.userError, RawDb $ Db.encode db)

            Right requestTableSecret ->
                if requestTableSecret == Row.getTableSecret top then
                    ( RawBody $
                       mconcat
                        [ RawBody.fetchedTable
                        , encodeRows (top : remainder)
                        ]
                    , RawDb $ Db.encode db
                    )

                else
                    (RawBody.userError, RawDb $ Db.encode db)


encodeRows :: [(RowId, RowSecret, Row)] -> Lazy.ByteString
encodeRows rows =
    mconcat $ map encodeRow rows
    
encodeRow :: (RowId, RowSecret, Row) -> Lazy.ByteString
encodeRow (rowId, rowSecret, row) =
    mconcat
    [ RowId.encode rowId
    , RowSecret.encode rowSecret
    , Row.encodeHttp row
    ]

thd :: (a, b, c) -> c
thd (_, _, c) =
    c

handleSaveRequest ::
  RequestSave ->
  TableSalt ->
  RowSalt ->
  Db ->
  (RawBody, RawDb)
handleSaveRequest save tableSalt rowSalt db =
  ( RawBody.ok,
    RawDb $
      Db.encode $
        Db.insertRow
          db
          (RequestSave.rowId save)
          (RequestSave.tableId save)
          tableSalt
          (RequestSave.tableSecret save)
          rowSalt
          (RequestSave.rowSecret save)
          (RequestSave.userData save)
  )

parseRandom :: RandomBytes -> Either RawBody (TableSalt, RowSalt, Nonce)
parseRandom (RandomBytes rawRandom) =
  case parseOnly parseRandomHelp rawRandom of
    Left _ ->
      Left RawBody.serverError
    Right random ->
      Right random

parseRandomHelp :: Parser (TableSalt, RowSalt, Nonce)
parseRandomHelp =
  do
    rowSalt <- RowSalt.parse
    tableSalt <- TableSalt.parse
    nonce <- Nonce.parse
    endOfInput
    return (tableSalt, rowSalt, nonce)

parse ::
  RandomBytes ->
  RawKey ->
  RawBody ->
  RawDb ->
  Either RawBody (Request, TableSalt, RowSalt, Db)
parse randomBytes rawKey rawBody rawDb =
  do
    (tableSalt, rowSalt, nonce) <- parseRandom randomBytes
    key <-
      case parseOnly Key.parse (RawKey.unwrap rawKey) of
        Left _ ->
          Left RawBody.serverError
        Right key_ ->
          Right key_
    request <-
      case parseOnly
        (Request.parse key nonce tableSalt rowSalt)
        (Lazy.toStrict (RawBody.get rawBody)) of
        Left _ ->
          Left RawBody.userError
        Right request_ ->
          Right request_

    db <-
      case parseOnly Db.parse (RawDb.get rawDb) of
        Left _ ->
          Left RawBody.serverError
        Right db_ ->
          Right db_

    return (request, tableSalt, rowSalt, db)
