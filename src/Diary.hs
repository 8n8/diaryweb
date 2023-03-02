module Diary (diary) where

import Request as R
import Response as T

diary :: R.Request -> T.Response
diary _ =
  T.Response
    { T.body = "",
      T.statusCode = 200,
      T.db = ""
    }
