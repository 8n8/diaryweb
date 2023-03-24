module Table exposing (Table, updateRow)

import AccessCode exposing (AccessCode)
import Dict exposing (Dict)
import Row exposing (Row)

type Table
    = Table
        { accessCode : AccessCode
        , rows : Rows
        }
    | NoSuchTable
    | Empty


updateRow : AccessCode -> Row -> Table -> Table
updateRow accessCode row oldTable =
    case oldTable of
        Table table ->
            Table { table | rows = Rows.updateRow accessCode row }

        NoSuch


type Rows
    = Loading
    | Loaded (Dict String Row)
