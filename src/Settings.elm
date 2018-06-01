module Settings exposing (Settings, decoder, fromJson, setDbName, setDbUrl)

import Data.DatabaseName as DatabaseName exposing (DatabaseName)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, required)


type alias Settings =
    { dbUrl : Maybe String
    , dbName : Maybe DatabaseName
    , apiEndPoint : Maybe String
    , grouped : Bool
    }


setDbUrl : Settings -> String -> Settings
setDbUrl settings dbUrl =
    { settings | dbUrl = Just dbUrl }


setDbName : Settings -> Maybe DatabaseName -> Settings
setDbName settings dbName =
    { settings | dbName = dbName }


decoder : Decoder Settings
decoder =
    decode Settings
        |> required "db_url" (Decode.nullable Decode.string)
        |> required "db_name" (Decode.nullable DatabaseName.decoder)
        |> required "api_endpoint" (Decode.nullable Decode.string)
        |> required "grouped" Decode.bool


fromJson : Value -> Settings
fromJson json =
    let
        settings =
            Decode.decodeValue decoder json
    in
    case settings of
        Ok settings ->
            settings

        Err e ->
            Debug.crash "Decoding settings should not be error" e
