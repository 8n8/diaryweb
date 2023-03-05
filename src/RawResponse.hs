module RawResponse (RawResponse, set) where

import RawBody (RawBody)
import RawStatus (RawStatus)

data RawResponse
  = RawResponse
      RawBody
      RawStatus

set :: RawBody -> RawStatus -> RawResponse
set body status =
  RawResponse body status
