module Response (Response (..)) where

import qualified Data.ByteString as B

data Response = Response
  { body :: B.ByteString,
    statusCode :: Int,
    db :: B.ByteString
  }
  deriving (Eq)

-- | This looks better in the test results than the default Show.
instance Show Response where
  show response =
    mconcat
      [ "Response { body = Data.ByteString.pack ",
        show $ B.unpack $ body response,
        ", statusCode = ",
        show $ statusCode response,
        ", db = Data.ByteString.pack ",
        show $ B.unpack $ db response,
        " }"
      ]
