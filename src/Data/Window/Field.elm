module Data.Window.Field
    exposing
        ( Field
        , FieldContainer(..)
        , FieldWidth(..)
        , cast
        , columnName
        , dataType
        , decoder
        , displayColumns
        , displayValues
        , dropdown
        , dropdownInfo
        , fieldDataTypes
        , firstColumnName
        , fontSize
        , shortOrLongWidth
        , sourceTable
        , widgetWidthListColumn
        , widgetWidthListValue
        )

import Data.Window.ColumnName as ColumnName exposing (ColumnName)
import Data.Window.DataType as DataType exposing (DataType)
import Data.Window.Record as Record exposing (Record, RecordId)
import Data.Window.TableName as TableName exposing (TableName)
import Data.Window.Value as Value exposing (Value)
import Data.Window.Widget as Widget exposing (ControlWidget, Dropdown, DropdownInfo)
import Dict
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, hardcoded, required)


type alias Field =
    { name : String
    , description : Maybe String
    , info : Maybe String
    , isPrimary : Bool
    , columnDetail : ColumnDetail
    , controlWidget : ControlWidget
    }


type FieldContainer
    = Detail
    | OneOne


dropdown : Field -> Maybe Dropdown
dropdown field =
    field.controlWidget.dropdown


dropdownInfo : Field -> Maybe DropdownInfo
dropdownInfo field =
    case dropdown field of
        Just (Widget.TableDropdown dropdownInfo) ->
            Just dropdownInfo

        Nothing ->
            Nothing


displayColumns : Field -> List ColumnName
displayColumns field =
    case dropdown field of
        Just (Widget.TableDropdown dropdown) ->
            dropdown.display.columns

        Nothing ->
            []


{-| only works for simple column name on fields
-}
tableColumn : Field -> TableName -> ColumnName -> String
tableColumn field tableName columnName =
    let
        columnName1 =
            firstColumnName field
    in
    case columnName1 of
        Just columnName1 ->
            columnName1.name ++ "." ++ tableName.name ++ "." ++ columnName.name

        Nothing ->
            tableName.name ++ "." ++ columnName.name


{-| Get a the dropdown record value
-}
displayValue : Field -> TableName -> ColumnName -> Record -> Maybe Value
displayValue field sourceTable displayColumn record =
    let
        columnName =
            tableColumn field sourceTable displayColumn
    in
    Dict.get columnName record


displayValues : Field -> Record -> Maybe String
displayValues field record =
    case dropdownInfo field of
        Just info ->
            let
                sourceTable =
                    info.source

                displayColumns =
                    info.display.columns

                separator =
                    Maybe.withDefault "" info.display.separator

                valueList =
                    List.filterMap
                        (\column ->
                            displayValue field sourceTable column record
                        )
                        displayColumns
            in
            if List.isEmpty valueList then
                Nothing
            else
                List.map Value.valueToString valueList
                    |> String.join separator
                    |> Just

        Nothing ->
            Nothing


sourceTable : Field -> Maybe TableName
sourceTable field =
    case dropdown field of
        Just (Widget.TableDropdown dropdown) ->
            Just dropdown.source

        Nothing ->
            Nothing


type ColumnDetail
    = Simple ( ColumnName, DataType )
    | Compound (List ( ColumnName, DataType ))


dataType : Field -> Maybe DataType
dataType field =
    case field.columnDetail of
        Simple ( columnName, dataType ) ->
            Just dataType

        Compound dataTypes ->
            case List.head dataTypes of
                Just ( columnName, dataType ) ->
                    Just dataType

                Nothing ->
                    Nothing


fieldDataTypes : Field -> List DataType
fieldDataTypes field =
    columnDataTypes field.columnDetail


cast : String -> Field -> Value
cast value field =
    case dataType field of
        Just dt ->
            case dt of
                DataType.Text ->
                    Value.Text value

                DataType.Int ->
                    Value.Int (forceInt value)

                DataType.Smallint ->
                    Value.Smallint (forceInt value)

                DataType.Tinyint ->
                    Value.Tinyint (forceInt value)

                DataType.Uuid ->
                    Value.Uuid value

                _ ->
                    Debug.crash ("unhandled casting of dataType " ++ toString dataType)

        Nothing ->
            Value.Nil


forceInt : String -> Int
forceInt value =
    case String.toInt value of
        Ok intValue ->
            intValue

        Err _ ->
            Debug.crash "this shouldn't happend"


columnDataTypes : ColumnDetail -> List DataType
columnDataTypes detail =
    case detail of
        Simple ( columnName, dataType ) ->
            [ dataType ]

        Compound listColumnDataType ->
            List.map
                (\( _, dataType ) ->
                    dataType
                )
                listColumnDataType


firstColumnName : Field -> Maybe ColumnName
firstColumnName field =
    case field.columnDetail of
        Simple ( columnName, _ ) ->
            Just columnName

        Compound detailList ->
            let
                columnName =
                    List.map
                        (\( columnName, _ ) ->
                            columnName
                        )
                        detailList
                        |> List.head
            in
            case columnName of
                Just columnName ->
                    Just columnName

                Nothing ->
                    Nothing


columnName : Field -> String
columnName field =
    case field.columnDetail of
        Simple ( columnName, _ ) ->
            ColumnName.completeName columnName

        Compound detailList ->
            List.map
                (\( columnName, _ ) ->
                    ColumnName.completeName columnName
                )
                detailList
                |> String.join " - "


decoder : Decoder Field
decoder =
    decode Field
        |> required "name" Decode.string
        |> required "description" (Decode.nullable Decode.string)
        |> required "info" (Decode.nullable Decode.string)
        |> required "is_primary" Decode.bool
        |> required "column_detail" columnDetailDecoder
        |> required "control_widget" Widget.controlWidgetDecoder


columnDetailDecoder : Decoder ColumnDetail
columnDetailDecoder =
    Decode.oneOf
        [ simpleColumnDecoder
        , compoundColumnDecoder
        ]


simpleColumnDecoder : Decoder ColumnDetail
simpleColumnDecoder =
    decode Simple
        |> required "Simple" columnPairDecoder


compoundColumnDecoder : Decoder ColumnDetail
compoundColumnDecoder =
    decode Compound
        |> required "Compound" (Decode.list columnPairDecoder)


columnPairDecoder : Decoder ( ColumnName, DataType )
columnPairDecoder =
    Decode.map2 (,)
        (Decode.index 0 ColumnName.decoder)
        (Decode.index 1 DataType.decoder)


widgetCharacterWidth : Field -> Int
widgetCharacterWidth field =
    let
        dateWidth =
            16

        columnLen =
            String.length (columnName field) + 5

        charWidth =
            case dataType field of
                Just dt ->
                    case dt of
                        DataType.Date ->
                            dateWidth

                        DataType.Timestamp ->
                            dateWidth

                        DataType.TimestampTz ->
                            dateWidth

                        _ ->
                            max columnLen field.controlWidget.width

                Nothing ->
                    max columnLen field.controlWidget.width
    in
    charWidth


fontSize : ( Int, Int )
fontSize =
    ( 12, 24 )


{-| Calculate the width, minimum 100, maximum 800
-}
widgetWidthListColumn : Field -> Int
widgetWidthListColumn field =
    let
        ( fontWidth, _ ) =
            fontSize

        calcWidth =
            widgetCharacterWidth field * fontWidth
    in
    clamp 100 800 calcWidth


widgetWidthListValue : Field -> Int
widgetWidthListValue field =
    widgetWidthListColumn field


type FieldWidth
    = Short
    | Long


{-|

    0 to 40 - 200px (Short)
    41 to ~ - 800px (Long)

-}
shortOrLongWidth : Field -> ( FieldWidth, Int )
shortOrLongWidth field =
    let
        width =
            field.controlWidget.width

        ( fontWidth, fontHeight ) =
            fontSize

        lines =
            round (toFloat width / 100) + 1
    in
    if width < 40 then
        ( Short, fontHeight )
    else
        ( Long, lines * fontHeight )
