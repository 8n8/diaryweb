module Indicator
  ( create,
    created,
    userError,
    serverError,
    got,
    get,
    delete,
    deleted,
  )
where

import Data.Word (Word8)
import Prelude ()

create :: Word8
create =
  0

created :: Word8
created =
  1

userError :: Word8
userError =
  2

serverError :: Word8
serverError =
  3

got :: Word8
got =
  4

get :: Word8
get =
  5

delete :: Word8
delete =
  6

deleted :: Word8
deleted =
  7
