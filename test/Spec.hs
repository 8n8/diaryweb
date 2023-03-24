module Main (main) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import Data.Text (Text, intercalate, lines, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Time
import qualified Data.Time.Clock
import qualified Data.Time.Clock.POSIX
import qualified Data.Time.Format
import Diary (diary)
import qualified HTMLEntities.Text
import qualified Hedgehog
import qualified Hedgehog.Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Types.Status
import Request
import Response
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.Hedgehog
import Prelude
  ( IO,
    Int,
    fromIntegral,
    map,
    mconcat,
    realToFrac,
    repeat,
    reverse,
    take,
    ($),
    (*),
    (<>),
  )

viewEntry :: (Int, Text) -> Text
viewEntry (timestamp, entry) =
  let prettyTime = formatTime timestamp
      prettyText =
        Data.Text.intercalate "</p><p>" $
          Prelude.map HTMLEntities.Text.text $
            lines entry
   in mconcat
        [ "    <h2>" <> prettyTime <> "</h1>\n",
          "    <p>" <> prettyText <> "</p>\n"
        ]

indexHtml :: Text
indexHtml =
  "<!DOCTYPE html>\n\
  \<html lang=\"en\">\n\
  \  <head>\n\
  \    <meta charset=\"utf-8\" />\n\
  \    <meta name=\"viewport\" content=\"width=device-width\" />\n\
  \    <title>Diary</title>\n\
  \    <style>\n\
  \      html {\n\
  \        font-family: sans-serif;\n\
  \      }\n\
  \      label {\n\
  \        font-size: 1.5rem;\n\
  \      }\n\
  \      form {\n\
  \        display: flex;\n\
  \        flex-direction: column;\n\
  \        row-gap: 0.5rem;\n\
  \      }\n\
  \      textarea {\n\
  \        font-family: sans-serif;\n\
  \        font-size: 1.5rem;\n\
  \      }\n\
  \      textarea:focus {\n\
  \        outline: blue solid 2px;\n\
  \      }\n\
  \      input {\n\
  \        width: fit-content;\n\
  \        font-size: 1.5rem;\n\
  \        font-family: sans-serif;\n\
  \      }\n\
  \      input:focus {\n\
  \        outline: blue solid 2px;\n\
  \      }\n\
  \      a:focus {\n\
  \        outline: blue solid 2px;\n\
  \      }\n\
  \      body {\n\
  \        display: flex;\n\
  \        flex-direction: column;\n\
  \        row-gap: 1rem;\n\
  \      }\n\
  \      ul {\n\
  \        display: flex;\n\
  \        flex-direction: row;\n\
  \        list-style-type: none;\n\
  \        padding: 0;\n\
  \        margin: 0;\n\
  \        column-gap: 1rem;\n\
  \        font-size: 1.5rem;\n\
  \      }\n\
  \    </style>\n\
  \  </head>\n\
  \  <body>\n\
  \    <nav>\n\
  \      <ul>\n\
  \        <li><a href=\"/read\">Read</a></li>\n\
  \        <li><a href=\"/\">Write</a></li>\n\
  \      </ul>\n\
  \    </nav>\n\
  \    <form action=\"/newentry\" method=\"POST\">\n\
  \      <label for=\"newentry\">Type a new diary entry here:</label>\n\
  \      <textarea\n\
  \        id=\"newentry\"\n\
  \        name=\"newentry\"\n\
  \        rows=\"5\"\n\
  \        cols=\"20\"></textarea>\n\
  \      <input type=\"submit\" value=\"Submit\">\n\
  \    </form>\n\
  \  </body>\n\
  \</html>\n\
  \"

favicon :: Lazy.ByteString
favicon =
  Lazy.pack
    [ 0x00,
      0x00,
      0x01,
      0x00,
      0x01,
      0x00,
      0x10,
      0x10,
      0x10,
      0x00,
      0x01,
      0x00,
      0x04,
      0x00,
      0x28,
      0x01,
      0x00,
      0x00,
      0x16,
      0x00,
      0x00,
      0x00,
      0x28,
      0x00,
      0x00,
      0x00,
      0x10,
      0x00,
      0x00,
      0x00,
      0x20,
      0x00,
      0x00,
      0x00,
      0x01,
      0x00,
      0x04,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0xed,
      0x29,
      0xdd,
      0x00,
      0xb6,
      0x6f,
      0x3e,
      0x00,
      0x0d,
      0xf4,
      0x0b,
      0x00,
      0x4a,
      0xf2,
      0xf5,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x33,
      0x33,
      0x11,
      0x11,
      0x11,
      0x11,
      0x23,
      0x33,
      0x33,
      0x33,
      0x11,
      0x11,
      0x11,
      0x11,
      0x23,
      0x33,
      0x33,
      0x33,
      0x11,
      0x11,
      0x11,
      0x11,
      0x23,
      0x33,
      0x33,
      0x33,
      0x11,
      0x11,
      0x11,
      0x11,
      0x23,
      0x33,
      0x00,
      0x00,
      0x11,
      0x10,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x11,
      0x10,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x11,
      0x10,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x11,
      0x10,
      0x00,
      0x00,
      0x00,
      0x00,
      0x33,
      0x33,
      0x11,
      0x11,
      0x11,
      0x11,
      0x23,
      0x33,
      0x33,
      0x33,
      0x11,
      0x11,
      0x11,
      0x11,
      0x23,
      0x33,
      0x33,
      0x33,
      0x11,
      0x11,
      0x11,
      0x11,
      0x23,
      0x33,
      0x33,
      0x33,
      0x11,
      0x11,
      0x11,
      0x11,
      0x23,
      0x33,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x11,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00
    ]

main :: IO ()
main =
  defaultMain $ testGroup "unit tests" $ Prelude.map oneTest cases

submittedHtml :: Text
submittedHtml =
  "<!DOCTYPE html>\n\
  \<html lang=\"en\">\n\
  \  <head>\n\
  \    <meta charset=\"utf-8\" />\n\
  \    <meta name=\"viewport\" content=\"width=device-width\" />\n\
  \    <title>Diary</title>\n\
  \    <style>\n\
  \      span {\n\
  \        font-family: sans-serif;\n\
  \        font-size: 1.5rem;\n\
  \      }\n\
  \      a:focus {\n\
  \        outline: blue solid 2px;\n\
  \      }\n\
  \      body {\n\
  \        display: flex;\n\
  \        flex-direction: column;\n\
  \        row-gap: 1rem;\n\
  \      }\n\
  \      ul {\n\
  \        display: flex;\n\
  \        flex-direction: row;\n\
  \        list-style-type: none;\n\
  \        padding: 0;\n\
  \        margin: 0;\n\
  \        column-gap: 1rem;\n\
  \        font-size: 1.5rem;\n\
  \      }\n\
  \    </style>\n\
  \  </head>\n\
  \  <body>\n\
  \    <nav>\n\
  \      <ul>\n\
  \        <li><a href=\"/read\">Read</a></li>\n\
  \        <li><a href=\"/\">Write</a></li>\n\
  \      </ul>\n\
  \    </nav>\n\
  \    <span>Your new diary entry has been saved.</span>\n\
  \  </body>\n\
  \</html>\n\
  \"

readEntriesHtml :: [(Int, Text)] -> Text
readEntriesHtml entries =
  mconcat
    [ "<!DOCTYPE html>\n\
      \<html lang=\"en\">\n\
      \  <head>\n\
      \    <meta charset=\"utf-8\" />\n\
      \    <meta name=\"viewport\" content=\"width=device-width\" />\n\
      \    <title>Diary</title>\n\
      \    <style>\n\
      \      html {\n\
      \        font-family: sans-serif;\n\
      \      }\n\
      \\n\
      \      h2 {\n\
      \        font-size: 1.5rem;\n\
      \        font-weight: 600;\n\
      \        margin: 0;\n\
      \      }\n\
      \      p {\n\
      \        font-size: 1.5rem;\n\
      \        font-weight: 400;\n\
      \      }\n\
      \      a:focus {\n\
      \        outline: blue solid 2px;\n\
      \      }\n\
      \      body {\n\
      \        display: flex;\n\
      \        flex-direction: column;\n\
      \        row-gap: 1rem;\n\
      \      }\n\
      \      ul {\n\
      \        display: flex;\n\
      \        flex-direction: row;\n\
      \        list-style-type: none;\n\
      \        padding: 0;\n\
      \        margin: 0;\n\
      \        column-gap: 1rem;\n\
      \        font-size: 1.5rem;\n\
      \      }\n\
      \    </style>\n\
      \  </head>\n\
      \  <body>\n\
      \    <nav>\n\
      \      <ul>\n\
      \        <li><a href=\"/read\">Read</a></li>\n\
      \        <li><a href=\"/\">Write</a></li>\n\
      \      </ul>\n\
      \    </nav>\n\
      \    <div>\n",
      mconcat (Prelude.map viewEntry (Prelude.reverse entries)),
      "    </div>\n\
      \  </body>\n\
      \</html>\n"
    ]

oneTest ::
  (TestName, Strict.ByteString, Request, (Response, Strict.ByteString)) ->
  TestTree
oneTest (name, db, request, expected) =
  testCase name $ diary db request @?= expected

cases :: [(TestName, Strict.ByteString, Request, (Response, Strict.ByteString))]
cases =
  [ ( "root route",
      "",
      Request
        { body = [],
          path = [],
          method = "GET",
          timestamp = 0
        },
      ( Response
          { body = fromStrict $ encodeUtf8 indexHtml,
            status = ok200,
            headers = [("Content-Type", "text/html")]
          },
        ""
      )
    ),
    ( "favicon route",
      "",
      Request
        { body = [],
          path = ["favicon.ico"],
          method = "GET",
          timestamp = 0
        },
      ( Response
          { body = favicon,
            status = ok200,
            headers = [("Content-Type", "image/vnd.microsoft.icon")]
          },
        ""
      )
    ),
    ( "new entry route simple",
      "",
      Request
        { body = [("newentry", "I ate a ham sandwich")],
          path = ["newentry"],
          timestamp = 89,
          method = "POST"
        },
      ( Response
          { body = fromStrict $ encodeUtf8 submittedHtml,
            status = ok200,
            headers = [("Content-Type", "text/html")]
          },
        (Strict.pack [89, 0, 0, 0] <> Strict.pack [20, 0, 0, 0] <> "I ate a ham sandwich")
      )
    )
  ]
