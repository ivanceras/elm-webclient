module Data.Window.GroupedWindow
    exposing
        ( GroupedWindow
        , WindowName
        , decoder
        , findMatch
        , windowNameDecoder
        )

import Data.Window as Window exposing (Window)
import Data.Window.TableName as TableName exposing (TableName)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import UrlParser


type alias GroupedWindow =
    { group : String
    , windowNames : List WindowName
    }


type alias WindowName =
    { name : String
    , tableName : TableName
    , isView : Bool
    }


findMatch : String -> GroupedWindow -> GroupedWindow
findMatch searchText groupedWindow =
    { groupedWindow | windowNames = matchWindowNames searchText groupedWindow.windowNames }


matchWindowNames : String -> List WindowName -> List WindowName
matchWindowNames searchText windowNames =
    List.filter
        (\windowName ->
            matchWindowName searchText windowName
        )
        windowNames


matchWindowName : String -> WindowName -> Bool
matchWindowName searchText windowName =
    String.startsWith searchText windowName.name
        || String.startsWith searchText windowName.tableName.name



-- SERIALIZATION --


decoder : Decoder GroupedWindow
decoder =
    decode GroupedWindow
        |> required "group" Decode.string
        |> required "window_names" (Decode.list windowNameDecoder)


windowNameDecoder : Decoder WindowName
windowNameDecoder =
    decode WindowName
        |> required "name" Decode.string
        |> required "table_name" TableName.decoder
        |> required "is_view" Decode.bool
