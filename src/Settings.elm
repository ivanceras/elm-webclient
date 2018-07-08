module Settings exposing (Settings, decoder, fromJson, setDbName, setDbUrl, setUsername, setPassword)

import Data.DatabaseName as DatabaseName exposing (DatabaseName)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, required, optional)



type alias Settings =
    { loginRequired : Bool
    , dbName : Maybe DatabaseName
    , apiEndPoint : Maybe String
    , grouped : Bool
    , cred: Maybe Cred
    }

type alias Cred = 
    { username: String
    , password: String
    }


setDbUrl : Settings -> Bool -> Settings
setDbUrl settings loginRequired =
    { settings | loginRequired = loginRequired }

credDecoder : Decoder Cred
credDecoder =
    decode Cred
        |> required "username" Decode.string
        |> required "password" Decode.string


setDbName : Settings -> Maybe DatabaseName -> Settings
setDbName settings dbName =
    { settings | dbName = dbName }

setUsername: Settings -> String -> Settings
setUsername settings username =
    let cred = case settings.cred of
        Just cred ->
            { cred | username = username }
        Nothing ->
            { username = username
            , password = ""
            }
    in
    { settings | cred = Just cred }

setPassword: Settings -> String -> Settings
setPassword settings password =
    let cred = case settings.cred of
        Just cred ->
            { cred | password = password }
        Nothing ->
            { username = ""
            , password = password
            }
    in
    { settings | cred = Just cred }


decoder : Decoder Settings
decoder =
    decode Settings
        |> optional "login_required" Decode.bool False
        |> required "db_name" (Decode.nullable DatabaseName.decoder)
        |> required "api_endpoint" (Decode.nullable Decode.string)
        |> required "grouped" Decode.bool
        |> optional "cred" (Decode.nullable credDecoder) Nothing


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
