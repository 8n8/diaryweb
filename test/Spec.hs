module Main (main) where

import qualified Data.ByteString as B
import Diary (diary)
import qualified Request as R
import qualified Response as T
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $ testGroup "Unit tests" $ map oneTest cases

oneTest :: Case -> TestTree
oneTest c =
  testCase (description c) $ diary (request c) @?= (response c)

rowId :: B.ByteString
rowId =
  B.pack [242, 198, 66, 109, 242, 50, 94, 88, 254, 123, 42, 156]

tableId :: B.ByteString
tableId =
  B.pack [133, 234, 53, 177, 68, 25, 184, 22, 153, 221, 220, 236]

data Case = Case
  { description :: String,
    request :: R.Request,
    response :: T.Response
  }

cases :: [Case]
cases =
  [ Case
      { description = "store a new entry item in the database",
        request =
          R.Request
            { R.path = ["save"],
              R.body = tableId <> rowId <> "Hi",
              R.db = ""
            },
        response =
          T.Response
            { T.body = "",
              T.statusCode = 200,
              T.db = tableId <> rowId <> B.pack [2, 0] <> "Hi"
            }
      }
  ]
