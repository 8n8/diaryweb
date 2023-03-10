module Indicator (create, created, userError, serverError) where

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
