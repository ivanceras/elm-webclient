module Views.Window.Field
    exposing
        ( Model
        , Msg(..)
        , calcWidgetSize
        , dropdownPageRequestNeeded
        , editedValue
        , init
        , isModified
        , listRecordToListString
        , update
        , view
        )

import Constant
import Data.Window.DataType as DataType exposing (DataType(..))
import Data.Window.Field as Field exposing (Field, FieldWidth)
import Data.Window.Lookup as Lookup exposing (Lookup)
import Data.Window.Presentation as Presentation exposing (Presentation(..))
import Data.Window.Record as Record exposing (Record)
import Data.Window.Tab as Tab exposing (Tab)
import Data.Window.TableName as TableName exposing (TableName)
import Data.Window.Value as Value exposing (ArrayValue(..), Value(..))
import Data.Window.Widget as Widget exposing (ControlWidget, DropdownInfo, Widget(..))
import Data.WindowArena as WindowArena exposing (Action(..))
import Date
import Date.Format
import Dict
import Html exposing (..)
import Html.Attributes exposing (checked, class, classList, for, id, name, selected, src, step, style, type_, value)
import Html.Events exposing (onBlur, onCheck, onClick, onFocus, onInput)
import Ionicon
import Route exposing (Route)
import Util exposing ((=>), Scroll, px)
import Widgets.DropdownDisplay as DropdownDisplay
import Widgets.FixDropdown as FixDropdown
import Widgets.Tagger as Tagger


type alias Model =
    { tab : Tab
    , field : Field
    , presentation : Presentation
    , record : Maybe Record
    , value : Maybe Value
    , widget : Widget
    , editValue : Maybe Value
    , dropdownInfo : Maybe DropdownInfo
    , allotedTabWidth : Int
    , isFocused : Bool
    , containerScroll : Scroll
    , lookup : Lookup
    }


{-|

    The edited value of this field model

-}
editedValue : Model -> Value
editedValue model =
    case model.editValue of
        Just value ->
            value

        Nothing ->
            Value.Nil


isModified : Model -> Bool
isModified model =
    model.value /= model.editValue


init : Int -> Presentation -> Action -> Maybe Record -> Tab -> Lookup -> Field -> Model
init allotedTabWidth presentation action record tab lookup field =
    let
        columnName =
            Field.columnName field

        origValue =
            case record of
                Just record ->
                    Dict.get columnName record

                Nothing ->
                    Nothing

        ( maybeValue, editValue ) =
            case action of
                Select _ ->
                    ( origValue, origValue )

                NewRecord _ ->
                    ( Nothing, Nothing )

                Copy _ ->
                    ( Nothing, origValue )

                ListPage ->
                    ( origValue, origValue )

        controlWidget =
            field.controlWidget

        dropdownInfo =
            case controlWidget.dropdown of
                Just (Widget.TableDropdown dropdownInfo) ->
                    Just dropdownInfo

                Nothing ->
                    Nothing

        widget =
            createWidget allotedTabWidth presentation record tab field editValue
    in
    { tab = tab
    , field = field
    , presentation = presentation
    , record = record
    , widget = widget
    , value = maybeValue
    , editValue = editValue
    , dropdownInfo = dropdownInfo
    , allotedTabWidth = allotedTabWidth
    , isFocused = False
    , containerScroll = Scroll 0 0
    , lookup = lookup
    }


view : Model -> Html Msg
view model =
    div
        [ class "widget-value"
        , classList [ ( "is-modified", isModified model ) ]
        ]
        [ viewWidget model ]


viewWidget : Model -> Html Msg
viewWidget model =
    case model.widget of
        HtmlWidget html ->
            html

        FixDropdown fixDropdown ->
            FixDropdown.view fixDropdown
                |> Html.map (FixDropdownMsg fixDropdown)

        TableDropdown dropdown ->
            let
                displayValue =
                    case model.record of
                        Just record ->
                            case Field.displayValues model.field record of
                                Just value ->
                                    value

                                Nothing ->
                                    ""

                        Nothing ->
                            ""

                dropdownInfo =
                    case model.dropdownInfo of
                        Just dropdownInfo ->
                            dropdownInfo

                        Nothing ->
                            Debug.crash "There should be dropdown info here"

                sourceTable =
                    dropdownInfo.source

                ( page, recordList ) =
                    Lookup.tableLookup sourceTable model.lookup

                list =
                    listRecordToListString dropdownInfo recordList

                listWithSelected =
                    case model.value of
                        Just pkValue ->
                            if
                                List.any
                                    (\( pk, display ) ->
                                        pk == pkValue
                                    )
                                    list
                            then
                                list
                            else
                                ( pkValue, displayValue ) :: list

                        Nothing ->
                            list
            in
            DropdownDisplay.view listWithSelected dropdown
                |> Html.map (DropdownDisplayMsg dropdown)


calcWidgetSize : Int -> Presentation -> Field -> ( FieldWidth, Int, Int )
calcWidgetSize allotedTabWidth presentation field =
    case presentation of
        InCard ->
            let
                ( fieldWidth, fieldHeight ) =
                    Field.shortOrLongWidth field
            in
            case fieldWidth of
                Field.Short ->
                    ( Field.Short, 200, fieldHeight )

                Field.Long ->
                    ( Field.Long, allotedTabWidth - 400, fieldHeight )

        InList ->
            let
                width =
                    Field.widgetWidthListValue field
            in
            ( Field.Short, width, 1 )


createWidget : Int -> Presentation -> Maybe Record -> Tab -> Field -> Maybe Value -> Widget
createWidget allotedTabWidth presentation record tab field maybeValue =
    let
        columnName =
            Field.columnName field

        recordId =
            case record of
                Just record ->
                    Just (Tab.recordId record tab)

                Nothing ->
                    Nothing

        recordIdString =
            case recordId of
                Just recordId ->
                    Record.idToString recordId

                Nothing ->
                    ""

        controlWidget =
            field.controlWidget

        widget =
            controlWidget.widget

        valueString =
            valueToString maybeValue

        maybeValueString =
            case maybeValue of
                Just Nil ->
                    Nothing

                Just value ->
                    Just (Value.valueToString value)

                Nothing ->
                    Nothing

        alignment =
            controlWidget.alignment

        alignmentString =
            alignment
                |> Widget.alignmentToString

        ( widthClass, widgetWidth, widgetHeight ) =
            calcWidgetSize allotedTabWidth presentation field

        styles =
            style
                [ ( "text-align", alignmentString )
                , ( "width", px widgetWidth )
                ]

        dataType =
            Field.dataType field
    in
    case widget of
        IntegerTextbox ->
            HtmlWidget
                (input
                    [ type_ "number"
                    , styles
                    , value valueString
                    , onInput IntegerValueChanged
                    , onFocus FieldFocused
                    , onBlur FieldBlurred
                    ]
                    []
                )

        DecimalTextbox ->
            HtmlWidget
                (input
                    [ type_ "number"
                    , styles

                    --TODO: step could be depending on the decimal places set in the column values
                    , step "any"
                    , value valueString
                    , onInput DecimalValueChanged
                    , onFocus FieldFocused
                    , onBlur FieldBlurred
                    ]
                    []
                )

        Textbox ->
            HtmlWidget
                (input
                    [ type_ "text"
                    , styles
                    , value valueString
                    , onInput StringValueChanged
                    , onFocus FieldFocused
                    , onBlur FieldBlurred
                    ]
                    []
                )

        PrimaryUrlLink ->
            let
                tableName =
                    tab.tableName
            in
            case presentation of
                InList ->
                    HtmlWidget
                        (div
                            [ class "primary-link-wrapper"
                            , styles
                            ]
                            [ a
                                [ class "primary-link"
                                , onClick (PrimaryLinkClicked tableName recordIdString)
                                , Route.href (Route.WindowArena (WindowArena.initArgWithRecordId tableName recordIdString))
                                , onFocus FieldFocused
                                , onBlur FieldBlurred
                                ]
                                [ text valueString ]
                            ]
                        )

                InCard ->
                    HtmlWidget
                        (input
                            [ type_ "text"
                            , styles
                            , value valueString
                            , onInput StringValueChanged
                            , onFocus FieldFocused
                            , onBlur FieldBlurred
                            ]
                            []
                        )

        MultilineText ->
            case presentation of
                InCard ->
                    HtmlWidget
                        (textarea
                            [ styles
                            , value valueString
                            , style [ ( "height", px widgetHeight ) ]
                            , style [ ( "min-height", px 24 ) ]
                            , style [ ( "min-width", px 100 ) ]
                            , onInput StringValueChanged
                            , onFocus FieldFocused
                            , onBlur FieldBlurred
                            ]
                            []
                        )

                InList ->
                    HtmlWidget
                        (input
                            [ type_ "text"
                            , styles
                            , value valueString
                            , onInput StringValueChanged
                            , onFocus FieldFocused
                            , onBlur FieldBlurred
                            ]
                            []
                        )

        UuidTextbox ->
            HtmlWidget
                (input
                    [ type_ "text"
                    , styles
                    , value valueString
                    , class "uuid-textbox"
                    , onInput StringValueChanged
                    , onFocus FieldFocused
                    , onBlur FieldBlurred
                    ]
                    []
                )

        Password ->
            HtmlWidget
                (input
                    [ type_ "password"
                    , styles
                    , value valueString
                    , onInput StringValueChanged
                    , onFocus FieldFocused
                    , onBlur FieldBlurred
                    ]
                    []
                )

        Checkbox ->
            let
                viewCheckbox =
                    case maybeValue of
                        Just argValue ->
                            let
                                checkedValue =
                                    case argValue of
                                        Value.Bool v ->
                                            checked v

                                        _ ->
                                            checked False
                            in
                            input
                                [ type_ "checkbox"
                                , checkedValue
                                , onCheck BoolValueChanged
                                , onFocus FieldFocused
                                , onBlur FieldBlurred
                                ]
                                []

                        Nothing ->
                            input
                                [ type_ "checkbox"
                                , onCheck BoolValueChanged
                                , onFocus FieldFocused
                                , onBlur FieldBlurred
                                ]
                                []
            in
            HtmlWidget
                (div
                    [ class "checkbox-value"
                    , styles
                    , onFocus FieldFocused
                    , onBlur FieldBlurred
                    ]
                    [ viewCheckbox ]
                )

        DateTimePicker ->
            HtmlWidget
                (viewDatePicker styles maybeValue)

        DatePicker ->
            HtmlWidget
                (viewDatePicker styles maybeValue)

        Widget.FixDropdown list ->
            let
                fixDropdownModel =
                    FixDropdown.init alignment widgetWidth maybeValueString list
            in
            FixDropdown fixDropdownModel

        TagSelection ->
            let
                tags =
                    case maybeValue of
                        Just value ->
                            case value of
                                Array arrayValue ->
                                    case arrayValue of
                                        TextArray list ->
                                            list

                                        IntArray list ->
                                            List.map toString list

                                        FloatArray list ->
                                            List.map toString list

                                _ ->
                                    []

                        Nothing ->
                            []
            in
            HtmlWidget
                (Tagger.view styles tags)

        FileUpload ->
            let
                iconColor =
                    Constant.iconColor

                iconSize =
                    20

                fileInputLabel =
                    "file-input-" ++ columnName

                rowFileInputLabel =
                    "file-input-" ++ recordIdString ++ "-" ++ columnName
            in
            case presentation of
                InList ->
                    HtmlWidget
                        (div
                            [ class "row-value-image"
                            , styles
                            ]
                            [ img [ src valueString ] []
                            , div [ class "image-upload" ]
                                [ label
                                    [ for rowFileInputLabel
                                    , class "tooltip"
                                    ]
                                    [ Ionicon.edit iconSize iconColor
                                    , span [ class "tooltip-text" ] [ text "Change image" ]
                                    ]
                                , input
                                    [ id rowFileInputLabel
                                    , type_ "file"
                                    , onFocus FieldFocused
                                    , onBlur FieldBlurred
                                    ]
                                    []
                                ]
                            ]
                        )

                InCard ->
                    HtmlWidget
                        (div
                            [ class "card-value-image"
                            ]
                            [ img [ src valueString ] []
                            , div [ class "image-upload" ]
                                [ label
                                    [ for fileInputLabel
                                    , class "tooltip"
                                    ]
                                    [ Ionicon.edit iconSize iconColor
                                    , span [ class "tooltip-text" ] [ text "Change image" ]
                                    ]
                                , input
                                    [ id fileInputLabel
                                    , type_ "file"
                                    , onFocus FieldFocused
                                    , onBlur FieldBlurred
                                    ]
                                    []
                                ]
                            ]
                        )

        Radiogroup list ->
            case presentation of
                InCard ->
                    HtmlWidget
                        (div []
                            (List.map
                                (\choice ->
                                    div []
                                        [ input
                                            [ type_ "radio"
                                            , name field.name
                                            , value choice
                                            , checked (choice == valueString)
                                            , id choice
                                            , onFocus FieldFocused
                                            , onBlur FieldBlurred
                                            ]
                                            []
                                        , label [ for choice ]
                                            [ text choice ]
                                        ]
                                )
                                list
                            )
                        )

                InList ->
                    let
                        listWithBlank =
                            "" :: list
                    in
                    HtmlWidget
                        (select [ styles ]
                            (List.map
                                (\v ->
                                    let
                                        isSelected =
                                            case maybeValue of
                                                Just fieldValue ->
                                                    v == Value.valueToString fieldValue

                                                Nothing ->
                                                    False
                                    in
                                    option
                                        [ value v
                                        , selected isSelected
                                        , onFocus FieldFocused
                                        , onBlur FieldBlurred
                                        ]
                                        [ text v ]
                                )
                                listWithBlank
                            )
                        )

        TableLookupDropdown ->
            let
                dropdownModel =
                    DropdownDisplay.init alignment widgetWidth maybeValue
            in
            TableDropdown dropdownModel

        AutocompleteDropdown ->
            let
                dropdownModel =
                    DropdownDisplay.init alignment widgetWidth maybeValue
            in
            TableDropdown dropdownModel

        _ ->
            Debug.crash ("unable to handle widget:" ++ toString controlWidget)


valueToString : Maybe Value -> String
valueToString maybeValue =
    case maybeValue of
        Just argValue ->
            Value.valueToString argValue

        Nothing ->
            ""


listRecordToListString : DropdownInfo -> List Record -> List ( Value, String )
listRecordToListString dropdownInfo lookupRecords =
    let
        displayColumns =
            dropdownInfo.display.columns

        separator =
            case dropdownInfo.display.separator of
                Just separator ->
                    separator

                Nothing ->
                    ""

        pk =
            dropdownInfo.display.pk
    in
    List.map
        (\record ->
            let
                displayValues : List Value
                displayValues =
                    List.filterMap
                        (\displayColumn ->
                            Dict.get displayColumn.name record
                        )
                        displayColumns

                displayString =
                    if List.isEmpty displayValues then
                        ""
                    else
                        List.map
                            (\value ->
                                Value.valueToString value
                            )
                            displayValues
                            |> String.join separator

                displayPk : List Value
                displayPk =
                    List.filterMap
                        (\pk ->
                            Dict.get pk.name record
                        )
                        pk

                onePk =
                    case List.head displayPk of
                        Just displayPk ->
                            displayPk

                        Nothing ->
                            Debug.crash "Only 1 pk is supported for now"
            in
            ( onePk, displayString )
        )
        lookupRecords


dropdownModel : Model -> Maybe DropdownDisplay.Model
dropdownModel model =
    case model.widget of
        TableDropdown dropdown ->
            Just dropdown

        _ ->
            Nothing


dropdownPageRequestNeeded : Model -> Maybe TableName
dropdownPageRequestNeeded model =
    case dropdownModel model of
        Just dropdown ->
            case Field.dropdownInfo model.field of
                Just dropdownInfo ->
                    let
                        sourceTable =
                            dropdownInfo.source

                        ( page, recordList ) =
                            Lookup.tableLookup sourceTable model.lookup

                        list =
                            listRecordToListString dropdownInfo recordList
                    in
                    if
                        DropdownDisplay.pageRequestNeeded list dropdown
                            && not (Lookup.hasReachedLastPage sourceTable model.lookup)
                    then
                        Just sourceTable
                    else
                        Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


viewDatePicker : Attribute Msg -> Maybe Value -> Html Msg
viewDatePicker styles maybeValue =
    let
        format =
            "%Y-%m-%dT%H:%M:%S"

        simpleFormat =
            "%Y-%m-%d"

        iso8601format v =
            Date.Format.format simpleFormat v

        dateString =
            case maybeValue of
                Just value ->
                    case value of
                        Nil ->
                            ""

                        Value.Timestamp v ->
                            iso8601format v

                        Value.Date v ->
                            iso8601format v

                        Value.DateTime v ->
                            iso8601format v

                        _ ->
                            Debug.crash ("This is not a supported date: " ++ toString value)

                Nothing ->
                    ""
    in
    input
        [ type_ "date"
        , styles
        , value dateString
        , onInput TimestampValueChanged
        , onFocus FieldFocused
        , onBlur FieldBlurred
        ]
        []


type Msg
    = DropdownDisplayMsg DropdownDisplay.Model DropdownDisplay.Msg
    | FixDropdownMsg FixDropdown.Model FixDropdown.Msg
    | StringValueChanged String
    | IntegerValueChanged String
    | DecimalValueChanged String
    | TimestampValueChanged String
    | BoolValueChanged Bool
    | ResetChanges
    | SetValue Value
    | PrimaryLinkClicked TableName String
    | FieldFocused
    | FieldBlurred
    | ContainerScrollChanged Scroll
    | LookupChanged Lookup
    | AllotedTabWidthChanged Int


type Widget
    = TableDropdown DropdownDisplay.Model
    | FixDropdown FixDropdown.Model
    | HtmlWidget (Html Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DropdownDisplayMsg dropdown (DropdownDisplay.SelectionChanged dropdownValue) ->
            let
                ( newDropdown, subCmd ) =
                    DropdownDisplay.update (DropdownDisplay.SelectionChanged dropdownValue) dropdown
            in
            { model
                | editValue = Just dropdownValue
                , widget = TableDropdown newDropdown
            }
                => Cmd.map (DropdownDisplayMsg newDropdown) subCmd

        DropdownDisplayMsg dropdown msg ->
            let
                ( newDropdown, subCmd ) =
                    DropdownDisplay.update msg dropdown
            in
            { model
                | widget = TableDropdown newDropdown
            }
                => Cmd.map (DropdownDisplayMsg newDropdown) subCmd

        FixDropdownMsg fixDropdown msg ->
            case model.widget of
                FixDropdown fixDropdown ->
                    let
                        ( newFix, subCmd ) =
                            FixDropdown.update msg fixDropdown
                    in
                    { model | widget = FixDropdown newFix }
                        => Cmd.map (FixDropdownMsg newFix) subCmd

                _ ->
                    model => Cmd.none

        IntegerValueChanged v ->
            let
                dataType =
                    Field.dataType model.field

                intValue =
                    String.toInt v

                value =
                    case dataType of
                        DataType.Tinyint ->
                            case intValue of
                                Ok f ->
                                    Value.Int f

                                Err e ->
                                    Value.Nil

                        DataType.Smallint ->
                            case intValue of
                                Ok f ->
                                    Value.Int f

                                Err e ->
                                    Value.Nil

                        DataType.Int ->
                            case intValue of
                                Ok f ->
                                    Value.Int f

                                Err e ->
                                    Value.Nil

                        DataType.Bigint ->
                            case intValue of
                                Ok f ->
                                    Value.Int f

                                Err e ->
                                    Value.Nil

                        _ ->
                            Value.Nil
            in
            { model | editValue = Just value }
                => Cmd.none

        DecimalValueChanged v ->
            let
                dataType =
                    Field.dataType model.field

                floatValue =
                    String.toFloat v

                value =
                    case dataType of
                        DataType.Double ->
                            case floatValue of
                                Ok f ->
                                    Value.Double f

                                Err e ->
                                    Value.Nil

                        DataType.Float ->
                            case floatValue of
                                Ok f ->
                                    Value.Float f

                                Err e ->
                                    Value.Nil

                        DataType.Real ->
                            case floatValue of
                                Ok f ->
                                    Value.Float f

                                Err e ->
                                    Value.Nil

                        DataType.Numeric ->
                            case floatValue of
                                Ok f ->
                                    Value.BigDecimal f

                                Err e ->
                                    Value.Nil

                        _ ->
                            Value.Nil
            in
            { model | editValue = Just value }
                => Cmd.none

        StringValueChanged v ->
            let
                dataType =
                    Field.dataType model.field

                value =
                    case dataType of
                        DataType.Double ->
                            case String.toFloat v of
                                Ok f ->
                                    Value.Double f

                                Err e ->
                                    Value.Nil

                        DataType.Float ->
                            case String.toFloat v of
                                Ok f ->
                                    Value.Float f

                                Err e ->
                                    Value.Nil

                        DataType.Int ->
                            case String.toInt v of
                                Ok f ->
                                    Value.Int f

                                Err e ->
                                    Value.Nil

                        DataType.Uuid ->
                            Value.Uuid v

                        _ ->
                            Value.Text v
            in
            { model | editValue = Just value }
                => Cmd.none

        TimestampValueChanged v ->
            let
                value =
                    case Date.fromString v of
                        Ok date ->
                            date

                        Err e ->
                            Debug.crash ("unable to parse date" ++ toString e)

                dateValue =
                    Value.Timestamp value
            in
            { model | editValue = Just dateValue }
                => Cmd.none

        BoolValueChanged v ->
            let
                value =
                    Value.Bool v
            in
            { model | editValue = Just value }
                => Cmd.none

        ResetChanges ->
            let
                updatedModel =
                    { model | editValue = model.value }
            in
            updateWidgetValue updatedModel updatedModel.value

        SetValue value ->
            let
                updatedModel =
                    { model
                        | editValue = Just value
                    }
            in
            updateWidgetValue updatedModel (Just value)

        -- this should be listened in the windowArena
        PrimaryLinkClicked tableName recordIdString ->
            model => Cmd.none

        FieldFocused ->
            { model | isFocused = True }
                => Cmd.none

        FieldBlurred ->
            { model | isFocused = False }
                => Cmd.none

        ContainerScrollChanged scroll ->
            let
                updatedModel =
                    { model | containerScroll = scroll }
                (updatedModel2, subCmd2) = updateDropdownDisplay (DropdownDisplay.ContainerScrollChanged scroll) updatedModel
                (updatedModel3, subCmd3) = updateFixDropdown (FixDropdown.ContainerScrollChanged scroll) updatedModel2
            in
                updatedModel3 => Cmd.batch [subCmd2, subCmd3]

        LookupChanged lookup ->
            { model | lookup = lookup }
                => Cmd.none

        AllotedTabWidthChanged width ->
            let
                updatedModel =
                    { model | allotedTabWidth = width }
            in
            -- TODO: Fix this, refactor updateWidgetValue to independently update the sizes
            updateWidgetValue updatedModel updatedModel.editValue


updateDropdownDisplay : DropdownDisplay.Msg -> Model -> ( Model, Cmd Msg )
updateDropdownDisplay dropdownMsg model =
    case model.widget of
        TableDropdown dropdown ->
            let
                ( updatedDropdown, subCmd ) =
                    DropdownDisplay.update dropdownMsg dropdown
            in
            { model | widget = TableDropdown updatedDropdown }
                => Cmd.map (DropdownDisplayMsg updatedDropdown) subCmd


        _ ->
            model => Cmd.none


updateFixDropdown : FixDropdown.Msg -> Model -> ( Model, Cmd Msg )
updateFixDropdown dropdownMsg model =
    case model.widget of
        FixDropdown dropdown ->
            let
                ( updatedDropdown, subCmd ) =
                    FixDropdown.update dropdownMsg dropdown
            in
            { model | widget = FixDropdown updatedDropdown }
                => Cmd.map (FixDropdownMsg updatedDropdown) subCmd
        _ ->
            model => Cmd.none


updateWidgetValue : Model -> Maybe Value -> ( Model, Cmd Msg )
updateWidgetValue model value =
    let
        widget =
            model.widget

        presentation =
            model.presentation

        record =
            model.record

        tab =
            model.tab

        field =
            model.field

        allotedTabWidth =
            model.allotedTabWidth
    in
    case widget of
        TableDropdown dropdown ->
            let
                updatedWidget =
                    case value of
                        Just value ->
                            let
                                ( updatedWidget, subCmd ) =
                                    DropdownDisplay.update (DropdownDisplay.SelectionChanged value) dropdown
                            in
                            TableDropdown updatedWidget

                        Nothing ->
                            widget
            in
            { model
                | widget = updatedWidget
            }
                => Cmd.none

        FixDropdown dropdown ->
            let
                updatedWidget =
                    case value of
                        Just value ->
                            let
                                ( updatedWidget, subCmd ) =
                                    case value of
                                        Value.Text text ->
                                            FixDropdown.update (FixDropdown.SelectionChanged text) dropdown

                                        _ ->
                                            Debug.crash "Fix dropdown other value is not supported"
                            in
                            FixDropdown updatedWidget

                        Nothing ->
                            widget
            in
            { model
                | widget = updatedWidget
            }
                => Cmd.none

        HtmlWidget html ->
            let
                updatedWidget =
                    createWidget allotedTabWidth presentation record tab field value
            in
            { model
                | widget = updatedWidget
            }
                => Cmd.none
