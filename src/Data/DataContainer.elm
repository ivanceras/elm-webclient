module Data.DataContainer exposing (..)

import Data.Window.Record as Record exposing (Record, RecordId, Rows)
import Data.Window.RecordDetail as RecordDetail exposing (RecordDetail)
import Data.Window.TableName as TableName exposing (TableName)
import Json.Encode as Encode


{-|

    Clicking on save button can mean:
     - save the newly inserted records into the database
     - save the modified records into the database

    TODO: need to consider the linked hasMany and indirect records

-}
type alias SaveContainer =
    { forInsert : ( TableName, List RecordDetailChangeset )
    , forUpdate : ( TableName, List RecordDetailChangeset )
    }


containerEncoder : SaveContainer -> Encode.Value
containerEncoder container =
    Encode.object
        [ ( "for_insert"
          , case container.forInsert of
                ( tableName, changesets ) ->
                    Encode.list
                        [ TableName.encoder tableName
                        , List.map changesetEncoder changesets
                            |> Encode.list
                        ]
          )
        , ( "for_update"
          , case container.forUpdate of
                ( tableName, changesets ) ->
                    Encode.list
                        [ TableName.encoder tableName
                        , List.map changesetEncoder changesets
                            |> Encode.list
                        ]
          )
        ]


{-|

    This is used when records have details which can be
     - unlink: remove the linkage of has_many/indirect record to the selected record
     - linkExisting: take the id of an existing has_many/indirect record and put it in the linker table
     - linkNew: create a new has_many/indirect record and put it's primary id to the linker table

-}
type RecordLinkAction
    = Unlink
    | LinkExisting
    | LinkNew
    | Edited


recordLinkActionEncoder : RecordLinkAction -> Encode.Value
recordLinkActionEncoder action =
    case action of
        Unlink ->
            Encode.string "Unlink"

        LinkExisting ->
            Encode.string "LinkExisting"

        LinkNew ->
            Encode.string "LinkNew"

        Edited ->
            Encode.string "Edited"


{-|

    Aside from the changes in the main record, changes in the detail record (has_many/indirect) record linked to this selected
    record will also have to be carried and saved into the database

-}
type alias RecordDetailChangeset =
    { record : Record
    , oneOnes : List ( TableName, Maybe Record )
    , hasMany : List ( TableName, RecordLinkAction, Rows )
    , indirect : List ( TableName, TableName, RecordLinkAction, Rows )
    }


changesetEncoder : RecordDetailChangeset -> Encode.Value
changesetEncoder changeset =
    Encode.object
        [ ( "record", Record.encoder changeset.record )
        , ( "one_ones"
          , List.map
                (\( tableName, record ) ->
                    Encode.list
                        [ TableName.encoder tableName
                        , case record of
                            Just record ->
                                Record.encoder record

                            Nothing ->
                                Encode.null
                        ]
                )
                changeset.oneOnes
                |> Encode.list
          )
        , ( "has_many"
          , List.map
                (\( tableName, action, rows ) ->
                    Encode.list
                        [ TableName.encoder tableName
                        , recordLinkActionEncoder action
                        , Record.rowsEncoder rows
                        ]
                )
                changeset.hasMany
                |> Encode.list
          )
        , ( "indirect"
          , List.map
                (\( tableName, via, action, rows ) ->
                    Encode.list
                        [ TableName.encoder tableName
                        , TableName.encoder via
                        , recordLinkActionEncoder action
                        , Record.rowsEncoder rows
                        ]
                )
                changeset.indirect
                |> Encode.list
          )
        ]
