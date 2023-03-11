module Request (Request (..), parse) where

import Capacity (Capacity)
import qualified Capacity
import Data.Attoparsec.ByteString (Parser, choice, endOfInput, word8)
import qualified Indicator
import Row (Row)
import qualified Row

data Request
  = Create Row
  | Get Capacity
  | Delete Capacity

parse :: Parser Request
parse =
  do
    request <-
      choice
        [ do
            _ <- word8 Indicator.create
            fmap Create Row.parse,
          do
            _ <- word8 Indicator.get
            fmap Get Capacity.parse,
          do
            _ <- word8 Indicator.delete
            fmap Delete Capacity.parse
        ]

    endOfInput
    return request
