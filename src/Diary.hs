module Diary (diary) where

import Crypto.Cipher.ChaChaPoly1305 (Nonce)
import Data.Attoparsec.ByteString (Parser, endOfInput, parseOnly)
import qualified Data.ByteString.Lazy as Lazy
import Db (Db)
import qualified Db
import qualified Key
import qualified Nonce
import RandomBytes (RandomBytes (RandomBytes))
import RawBody (RawBody)
import qualified RawBody
import RawDb (RawDb (RawDb))
import qualified RawDb
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
    tableSalt <- TableSalt.parse
    rowSalt <- RowSalt.parse
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
