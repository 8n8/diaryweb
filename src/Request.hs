module Request (Request (..), parse) where

import Crypto.Cipher.ChaChaPoly1305 (Nonce)
import Data.Attoparsec.ByteString (Parser, choice, endOfInput)
import Key (Key)
import RequestSave (RequestSave)
import RequestFetchTable (RequestFetchTable)
import qualified RequestFetchTable
import qualified RequestSave
import RowSalt (RowSalt)
import TableSalt (TableSalt)

data Request
  = Save RequestSave
  | FetchTable RequestFetchTable

parse :: Key -> Nonce -> TableSalt -> RowSalt -> Parser Request
parse key nonce tableSalt rowSalt =
  do
    request <-
      choice
        [ fmap Save (RequestSave.parse key nonce tableSalt rowSalt)
        , fmap FetchTable RequestFetchTable.parse
        ]

    endOfInput
    return request
