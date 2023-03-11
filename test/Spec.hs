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

capacity2 :: ByteString
capacity2 =
  Strict.pack
    [84, 51, 137, 208, 221, 179, 135, 28, 254, 102, 254, 141, 173, 234, 77, 158]

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
        bodyOut =
          RawBody $
            mconcat
              [ Lazy.singleton Indicator.created,
                Lazy.fromStrict capacity
              ],
        dbOut = RawDb $ mconcat [capacity, Strict.pack [2, 0], "Hi"]
      },
    Case
      { description = "get with single item",
        bodyIn =
          RawBody $
            Lazy.fromStrict $
              Strict.singleton Indicator.get <> capacity,
        dbIn = RawDb $ mconcat [capacity, Strict.pack [2, 0], "Hi"],
        bodyOut =
          RawBody $
            mconcat
              [ Lazy.singleton Indicator.got,
                Lazy.fromStrict capacity,
                Lazy.pack [2, 0],
                "Hi"
              ],
        dbOut = RawDb $ mconcat [capacity, Strict.pack [2, 0], "Hi"]
      },
    Case
      { description = "get with two items in DB, one matching",
        bodyIn =
          RawBody $
            Lazy.fromStrict $
              Strict.singleton Indicator.get <> capacity,
        dbIn =
          RawDb $
            mconcat
              [ capacity,
                Strict.pack [2, 0],
                "Hi",
                capacity2,
                Strict.pack [3, 0],
                "Hey"
              ],
        bodyOut =
          RawBody $
            mconcat
              [ Lazy.singleton Indicator.got,
                Lazy.fromStrict capacity,
                Lazy.pack [2, 0],
                "Hi"
              ],
        dbOut =
          RawDb $
            mconcat
              [ capacity,
                Strict.pack [2, 0],
                "Hi",
                capacity2,
                Strict.pack [3, 0],
                "Hey"
              ]
      },
    Case
      { description = "get with two items in DB, both matching",
        bodyIn =
          RawBody $
            Lazy.fromStrict $
              Strict.singleton Indicator.get <> capacity,
        dbIn =
          RawDb $
            mconcat
              [ capacity,
                Strict.pack [3, 0],
                "Hey",
                capacity,
                Strict.pack [2, 0],
                "Hi"
              ],
        bodyOut =
          RawBody $
            mconcat
              [ Lazy.singleton Indicator.got,
                Lazy.fromStrict capacity,
                Lazy.pack [3, 0],
                "Hey",
                Lazy.fromStrict capacity,
                Lazy.pack [2, 0],
                "Hi"
              ],
        dbOut =
          RawDb $
            mconcat
              [ capacity,
                Strict.pack [3, 0],
                "Hey",
                capacity,
                Strict.pack [2, 0],
                "Hi"
              ]
      }
  ]
