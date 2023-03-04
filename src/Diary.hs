module Diary (diary) where

import qualified Request
import qualified Response
import qualified UserData
import RowId
import TableId

diary :: Request.Request -> Response.Response
diary request =
  case parseRequest request of
     Left err ->
        Left err

     Right parsed ->
       handleValidRequest parsed

parseRequest :: Parsec.Parser Request
parseRequest =
    do
    tableId <- TableId.parseHttp
    rowId <- RowId.parseHttp

data Request
    = Save TableId SessionSecret UserData 

handleValidRequest :: Request -> Response.Response
handleValidRequest parsed =
  Response.Response
    { Response.body = "",
      Response.statusCode = 200,
      Response.db = ""
    }
