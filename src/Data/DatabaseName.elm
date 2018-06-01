module Data.DatabaseName
    exposing
        ( DatabaseName
        , decoder
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, hardcoded, required)


type alias DatabaseName =
    { name : String
    , description : Maybe String
    }


decoder : Decoder DatabaseName
decoder =
    decode DatabaseName
        |> required "name" Decode.string
        |> required "description" (Decode.nullable Decode.string)
