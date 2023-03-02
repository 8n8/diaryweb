module Request (Request (..)) where

import qualified Data.ByteString as B
import qualified Data.Text as T

data Request = Request
  { path :: [T.Text],
    body :: B.ByteString,
    db :: B.ByteString
  }
