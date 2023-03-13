module Diary (diary) where

import Capacity (Capacity)
import Data.Attoparsec.ByteString (parseOnly)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Set as Set
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
      handleCreateRequest row db
    Request.Get capacity ->
      (handleGetRequest capacity db, RawDb $ Db.encode db)
    Request.Delete capacity ->
      handleDeleteRequest capacity db

handleCreateRequest :: Row -> Db -> (RawBody, RawDb)
handleCreateRequest row db =
  ( RawBody "",
    RawDb $ Db.encode $ Db.insert row db
  )

handleDeleteRequest :: Capacity -> Db -> (RawBody, RawDb)
handleDeleteRequest capacity db =
  ( RawBody "",
    RawDb $ Db.encode $ Db.delete capacity db
  )

handleGetRequest :: Capacity -> Db -> RawBody
handleGetRequest capacity db =
  let matching = Set.toList $ Db.get capacity db
   in RawBody $
        Lazy.fromStrict $
          mconcat $
            encodeUint32 (length matching) : map Row.encodeHttp matching

encodeUint32 :: Int -> Strict.ByteString
encodeUint32 i =
  Strict.pack
    [ fromIntegral $ i .&. 0xff,
      fromIntegral $ (shiftR i 8) .&. 0xff,
      fromIntegral $ (shiftR i 16) .&. 0xff,
      fromIntegral $ (shiftR i 24) .&. 0xff
    ]

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
