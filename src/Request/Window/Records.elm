module Request.Window.Records
    exposing
        ( delete
        , fetchHasManyRecords
        , fetchIndirectRecords
        , fetchSelected
        , header
        , listPageWithQuery
        , lookupPage
        , lookups
        )

import Data.AuthToken as AuthToken exposing (AuthToken, withAuthorization)
import Data.DataContainer
import Data.Query as Query exposing (Query)
import Data.Query.Sort as Sort exposing (Sort)
import Data.Window as Window exposing (Tag, Window, slugToString)
import Data.Window.GroupedWindow as GroupedWindow exposing (WindowName)
import Data.Window.Lookup as Lookup exposing (Lookup)
import Data.Window.Record as Record exposing (RecordId, Rows)
import Data.Window.RecordDetail as RecordDetail exposing (RecordDetail)
import Data.Window.TableName as TableName
    exposing
        ( TableName
        , tableNameParser
        , tableNameToString
        )
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Request.Helpers exposing (apiUrl)
import Settings exposing (Settings)
import Util exposing ((=>))


-- LIST --


header : Settings -> RequestBuilder a -> RequestBuilder a
header settings request =
    let
        cred =
            settings.cred
    in
    case cred of
        Just cred ->
            HttpBuilder.withHeaders 
                [("username", cred.username), ("password", cred.password)]
                request

        Nothing ->
            request


listPageWithQuery : Settings -> Maybe AuthToken -> TableName -> Query -> Http.Request Rows
listPageWithQuery settings maybeToken tableName query =
    let
        queryStr =
            Query.mainQueryToString query
    in
    apiUrl settings ("/data/" ++ tableNameToString tableName ++ "/" ++ queryStr)
        |> HttpBuilder.get
        |> header settings
        |> HttpBuilder.withExpect (Http.expectJson Record.rowsDecoder)
        |> withAuthorization maybeToken
        |> HttpBuilder.toRequest


lookups : Settings -> Maybe AuthToken -> TableName -> Http.Request Lookup
lookups settings maybeToken tableName =
    apiUrl settings ("/lookup_all/" ++ tableNameToString tableName)
        |> HttpBuilder.get
        |> header settings
        |> HttpBuilder.withExpect (Http.expectJson Lookup.decoder)
        |> withAuthorization maybeToken
        |> HttpBuilder.toRequest


lookupPage : Settings -> Int -> Maybe AuthToken -> TableName -> Http.Request Rows
lookupPage settings page maybeToken tableName =
    apiUrl settings ("/lookup/" ++ tableNameToString tableName ++ "/" ++ toString page)
        |> HttpBuilder.get
        |> header settings
        |> HttpBuilder.withExpect (Http.expectJson Record.rowsDecoder)
        |> withAuthorization maybeToken
        |> HttpBuilder.toRequest


fetchSelected : Settings -> TableName -> String -> Http.Request RecordDetail
fetchSelected settings tableName selectedRow =
    apiUrl settings ("/select/" ++ tableNameToString tableName ++ "/" ++ selectedRow)
        |> HttpBuilder.get
        |> header settings
        |> HttpBuilder.withExpect (Http.expectJson RecordDetail.decoder)
        |> HttpBuilder.toRequest


fetchHasManyRecords : Settings -> TableName -> String -> TableName -> Query -> Http.Request Rows
fetchHasManyRecords settings tableName selectedRow hasManyTable query =
    let
        queryStr =
            Query.mainQueryToString query
    in
    apiUrl settings
        ("/has_many_select/"
            ++ tableNameToString tableName
            ++ "/"
            ++ selectedRow
            ++ "/"
            ++ tableNameToString hasManyTable
            ++ "/"
            ++ queryStr
        )
        |> HttpBuilder.get
        |> header settings
        |> HttpBuilder.withExpect (Http.expectJson Record.rowsDecoder)
        |> HttpBuilder.toRequest


fetchIndirectRecords : Settings -> TableName -> String -> TableName -> Query -> Http.Request Rows
fetchIndirectRecords settings tableName selectedRow hasManyTable query =
    let
        queryStr =
            Query.mainQueryToString query
    in
    apiUrl settings
        ("/indirect_select/"
            ++ tableNameToString tableName
            ++ "/"
            ++ selectedRow
            ++ "/"
            ++ tableNameToString hasManyTable
            ++ "/"
            ++ queryStr
        )
        |> HttpBuilder.get
        |> header settings
        |> HttpBuilder.withExpect (Http.expectJson Record.rowsDecoder)
        |> HttpBuilder.toRequest


delete : Settings -> TableName -> RecordId -> AuthToken -> Http.Request ()
delete settings tableName recordId token =
    apiUrl settings ("/window/" ++ tableNameToString tableName ++ "/data/" ++ Record.idToString recordId)
        |> HttpBuilder.delete
        |> header settings
        |> withAuthorization (Just token)
        |> HttpBuilder.toRequest
