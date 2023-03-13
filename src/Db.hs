module Db (Db, parse, insert, encode, empty, get, delete) where

import Capacity (Capacity)
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as Parsec
import Data.ByteString (ByteString)
import Data.Set (Set)
import qualified Data.Set as Set
import Row (Row)
import qualified Row

newtype Db
  = Db (Set Row)

encode :: Db -> ByteString
encode (Db rows) =
  mconcat $ map Row.encode $ Set.toList rows

delete :: Capacity -> Db -> Db
delete capacity (Db rows) =
  Db (Set.filter (\row -> Row.capacity row /= capacity) rows)

insert :: Row -> Db -> Db
insert row (Db rows) =
  Db (Set.insert row rows)

parse :: Parser Db
parse =
  do
    rows <- Parsec.many' Row.parse
    return $ Db (Set.fromList rows)

empty :: Db
empty =
  Db Set.empty

get :: Capacity -> Db -> Set Row
get capacity (Db rows) =
  Set.filter (\row -> Row.capacity row == capacity) rows
