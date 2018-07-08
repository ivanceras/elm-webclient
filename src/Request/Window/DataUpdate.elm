module Request.Window.DataUpdate exposing (..)

import Data.AuthToken as AuthToken exposing (AuthToken, withAuthorization)
import Data.DataContainer as DataContainer exposing (RecordDetailChangeset, SaveContainer)
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
import Json.Encode as Encode
import Request.Window.Records exposing (header)
import Request.Helpers exposing (apiUrl)
import Settings exposing (Settings)


updateRecord : Settings -> Maybe AuthToken -> TableName -> RecordDetailChangeset -> Http.Request RecordDetail
updateRecord settings maybeToken tableName recordChangeset =
    let
        jsonBody =
            DataContainer.changesetEncoder recordChangeset

        _ =
            Debug.log "sending: " jsonBody
    in
    apiUrl settings ("/record_changeset/" ++ tableNameToString tableName)
        |> HttpBuilder.post
        |> HttpBuilder.withJsonBody jsonBody
        |> header settings
        |> HttpBuilder.withExpect (Http.expectJson RecordDetail.decoder)
        |> withAuthorization maybeToken
        |> HttpBuilder.toRequest


updateTab : Settings -> Maybe AuthToken -> TableName -> SaveContainer -> Http.Request Rows
updateTab settings maybeToken tableName container =
    let
        jsonBody =
            DataContainer.containerEncoder container

        _ =
            Debug.log "sending: " jsonBody
    in
    apiUrl settings "/tab_changeset/"
        |> HttpBuilder.post
        |> HttpBuilder.withJsonBody jsonBody
        |> header settings
        |> HttpBuilder.withExpect (Http.expectJson Record.rowsDecoder)
        |> withAuthorization maybeToken
        |> HttpBuilder.toRequest
