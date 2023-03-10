module Diary (diary) where

import qualified Capacity
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString.Lazy as Lazy
import Db (Db)
import qualified Db
import qualified Indicator
import RawBody (RawBody (RawBody))
import qualified RawBody
import RawDb (RawDb (RawDb))
import qualified RawDb
import Request (Request)
import qualified Request
import Row (Row)
import qualified Row

diary :: RawBody -> RawDb -> (RawBody, RawDb)
diary rawBody rawDb =
  case parse rawBody rawDb of
    Left err ->
      (err, rawDb)
    Right (request, db) ->
      handleValidRequest request db

handleValidRequest :: Request -> Db -> (RawBody, RawDb)
handleValidRequest request db =
  case request of
    Request.Create row ->
      handleSaveRequest row db

handleSaveRequest :: Row -> Db -> (RawBody, RawDb)
handleSaveRequest row db =
  ( RawBody $
      mconcat
        [ Lazy.singleton Indicator.created,
          Lazy.fromStrict $ Capacity.encode $ Row.capacity row
        ],
    RawDb $ Db.encode $ Db.insert row db
  )

parse :: RawBody -> RawDb -> Either RawBody (Request, Db)
parse rawBody rawDb =
  do
    request <-
      case parseOnly
        Request.parse
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

    return (request, db)
