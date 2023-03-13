module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Diary (diary)
import qualified Indicator
import RawBody (RawBody (RawBody))
import RawDb (RawDb (RawDb))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main =
  defaultMain $ testGroup "Unit tests" $ map oneTest cases

oneTest :: Case -> TestTree
oneTest c =
  testCase (description c) $
    diary (bodyIn c) (dbIn c) @?= (bodyOut c, dbOut c)

data Case = Case
  { description :: String,
    bodyIn :: RawBody,
    dbIn :: RawDb,
    bodyOut :: RawBody,
    dbOut :: RawDb
  }

capacity :: ByteString
capacity =
  Strict.pack
    [82, 16, 17, 204, 170, 207, 205, 124, 167, 152, 128, 61, 227, 6, 198, 100]

cases :: [Case]
cases =
  [ Case
      { description = "create",
        bodyIn =
          RawBody $
            Lazy.fromStrict $
              mconcat
                [ Strict.singleton Indicator.create,
                  capacity,
                  Strict.pack [2, 0],
                  "Hi"
                ],
        dbIn = RawDb "",
        bodyOut = RawBody (Lazy.singleton Indicator.created),
        dbOut = RawDb $ mconcat [capacity, Strict.pack [2, 0], "Hi"]
      }
  ]
