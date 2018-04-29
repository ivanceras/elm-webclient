module Data.Window.Record
    exposing
        ( Record
        , RecordId(..)
        , Rows
        , decoder
        , empty
        , emptyRow
        , encoder
        , idToString
        , listRecordToRows
        , rowsDecoder
        , rowsEncoder
        , rowsToRecordList
        )

import Data.Window.Author as Author exposing (Author)
import Data.Window.DataType as DataType exposing (DataType)
import Data.Window.TableName as TableName exposing (TableName)
import Data.Window.Value as Value exposing (Value(..))
import Date exposing (Date)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode


type alias Rows =
    { columns : List String
    , data : List (List Value)
    , count : Maybe Int
    }


rowsEncoder : Rows -> Encode.Value
rowsEncoder rows =
    let
        data =
            List.map
                (\d ->
                    List.map
                        (\c ->
                            Value.encoder c
                        )
                        d
                        |> Encode.list
                )
                rows.data
                |> Encode.list
    in
    Encode.object
        [ ( "columns", Encode.list (List.map Encode.string rows.columns) )
        , ( "data", data )
        , ( "count"
          , case rows.count of
                Just count ->
                    Encode.int count

                Nothing ->
                    Encode.null
          )
        ]


emptyRow : Rows
emptyRow =
    { columns = []
    , data = []
    , count = Nothing
    }


type alias Record =
    Dict String Value


empty =
    Dict.empty


encoder : Record -> Encode.Value
encoder record =
    let
        list =
            Dict.toList record
    in
    List.map
        (\( k, v ) ->
            ( k, Value.encoder v )
        )
        list
        |> Encode.object


rowsToRecordList : Rows -> List Record
rowsToRecordList rows =
    List.map
        (\data ->
            List.map2 (,) rows.columns data
                |> Dict.fromList
        )
        rows.data


listRecordToRows : List String -> List Record -> Rows
listRecordToRows columns listRecord =
    let
        data =
            List.map
                (\record ->
                    List.map
                        (\column ->
                            case Dict.get column record of
                                Just value ->
                                    value

                                Nothing ->
                                    Value.Nil
                        )
                        columns
                )
                listRecord
    in
    { columns = columns
    , data = data
    , count = Nothing
    }



-- SERIALIZATION --


decoder : Decoder Record
decoder =
    Decode.dict Value.decoder


rowsDecoder : Decoder Rows
rowsDecoder =
    decode Rows
        |> required "columns" (Decode.list Decode.string)
        |> required "data" (Decode.list (Decode.list Value.decoder))
        |> required "count" (Decode.nullable Decode.int)



-- IDENTIFIERS --


type RecordId
    = RecordId (List Value)
    | TempLocal Int


idToString : RecordId -> String
idToString recordId =
    case recordId of
        RecordId values ->
            List.map Value.valueToString values
                |> String.join ","

        TempLocal v ->
            toString v



{-
   parseRecordId : String -> List DataType -> Maybe RecordId
   parseRecordId arg dataTypes =
       let
           args : List String
           args =
               String.split "," arg

           values : List (Maybe Value)
           values =
               List.map2
                   (\arg dataType ->
                       let
                           parsedValues : Maybe Value
                           parsedValues =
                               valueFromString arg dataType
                       in
                           parsedValues
                   )
                   args
                   dataTypes

           recordValues : List Value
           recordValues =
               List.filterMap (\v -> v) values
       in
           case List.isEmpty recordValues of
               False ->
                   Just (RecordId recordValues)

               True ->
                   Nothing
-}
{--
valueFromString : String -> DataType -> Maybe Value
valueFromString arg dataType =
    case dataType of
        DataType.Tinyint ->
            case String.toInt arg of
                Ok v ->
                    Just (Tinyint v)

                Err e ->
                    Nothing

        DataType.Smallint ->
            case String.toInt arg of
                Ok v ->
                    Just (Smallint v)

                Err e ->
                    Nothing

        DataType.Int ->
            case String.toInt arg of
                Ok v ->
                    Just (Int v)

                Err e ->
                    Nothing

        DataType.Bigint ->
            case String.toInt arg of
                Ok v ->
                    Just (Bigint v)

                Err e ->
                    Nothing

        DataType.Text ->
            case String.isEmpty arg of
                True ->
                    Nothing

                False ->
                    Just (Text arg)

        DataType.Uuid ->
            Just (Uuid arg)

        _ ->
            Debug.crash ("This is not dealt with yet: " ++ arg ++ " " ++ (toString dataType))
                Nothing
--}
