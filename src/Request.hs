module Request (Request (..), parse) where

import Data.Attoparsec.ByteString (Parser, choice, endOfInput, word8)
import qualified Indicator
import Row (Row)
import qualified Row

data Request
  = Create Row

parse :: Parser Request
parse =
  do
    request <-
      choice
        [ do
            _ <- word8 Indicator.create
            fmap Create Row.parse
        ]

    endOfInput
    return request
