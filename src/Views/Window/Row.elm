module Views.Window.Row
    exposing
        ( Model
        , Msg(..)
        , dropdownPageRequestNeeded
        , editedRecord
        , init
        , isModified
        , update
        , view
        , viewRowControls
        )

import Constant
import Data.Window.Field as Field exposing (Field)
import Data.Window.Lookup as Lookup exposing (Lookup)
import Data.Window.Presentation as Presentation exposing (Presentation(..))
import Data.Window.Record as Record exposing (Record, RecordId)
import Data.Window.Tab as Tab exposing (Tab)
import Data.Window.TableName exposing (TableName)
import Data.Window.Value as Value exposing (Value)
import Data.Window.Widget as Widget exposing (ControlWidget)
import Data.WindowArena as WindowArena exposing (Action(..))
import Dict
import Html exposing (..)
import Html.Attributes exposing (attribute, checked, class, classList, href, id, placeholder, src, style, type_)
import Html.Events exposing (onCheck, onClick)
import Ionicon
import Material.Icons.Action as MaterialAction
import Material.Icons.Content as MaterialContent
import Material.Icons.Editor as MaterialEditor
import Material.Icons.Maps as MaterialMaps
import Route exposing (Route)
import Util exposing ((=>), Scroll, pair, px, viewIf)
import Views.Window.Field as Field


type alias Model =
    { selected : Bool
    , recordId : RecordId
    , record : Record
    , tab : Tab
    , fields : List Field.Model
    , containerScroll : Scroll
    , lookup : Lookup
    }


isRowFocused : Model -> Bool
isRowFocused model =
    List.any .isFocused
        model.fields


{-|

    Get the edited records for each field

-}
editedRecord : Model -> Record
editedRecord model =
    List.map
        (\field ->
            let
                value =
                    Field.editedValue field

                columnName =
                    Field.columnName field.field
            in
            ( columnName, value )
        )
        model.fields
        |> Dict.fromList


isModified : Model -> Bool
isModified model =
    List.any Field.isModified model.fields


init : Action -> RecordId -> Record -> Tab -> Lookup -> Model
init action recordId record tab lookup =
    { selected = False
    , recordId = recordId
    , record = record
    , tab = tab
    , fields = createFields action record tab lookup
    , containerScroll = Scroll 0 0
    , lookup = lookup
    }


createFields : Action -> Record -> Tab -> Lookup -> List Field.Model
createFields action record tab lookup =
    List.map
        (Field.init 0 InList action (Just record) tab lookup)
        tab.fields


view : Model -> Html Msg
view model =
    let
        recordId =
            model.recordId

        record =
            model.record

        tab =
            model.tab

        fields =
            tab.fields

        -- rearrange fields here if needed
    in
    div [ class "tab-row-wrapper" ]
        [ div
            [ class "tab-row"
            , classList
                [ ( "is-modified", isModified model )
                , ( "row-focused", isRowFocused model )
                ]
            ]
            (List.map
                (\value ->
                    let
                        ( widthClass, widgetWidth, widgetHeight ) =
                            Field.calcWidgetSize 0 Presentation.InList value.field

                        rowWidth =
                            widgetWidth + Constant.columnPad
                    in
                    div
                        [ class "tab-row-value"
                        , Constant.tabRowValueStyle
                        , style [ ( "width", px rowWidth ) ]
                        ]
                        [ Field.view value
                            |> Html.map (FieldMsg value)
                        ]
                )
                model.fields
            )
        ]


viewRowControls : Model -> RecordId -> Tab -> Html Msg
viewRowControls model recordId tab =
    div
        [ class "row-controls"
        ]
        [ viewSelectionControl model
        , viewRecordDetail recordId tab
        , viewCopyControl recordId tab
        , viewUndo model
        , viewSave model
        ]


viewSelectionControl : Model -> Html Msg
viewSelectionControl model =
    div [ class "row-select tooltip" ]
        [ input
            [ type_ "checkbox"
            , onCheck ToggleSelect
            , checked model.selected
            ]
            []
        , span [ class "tooltip-text" ]
            [ text "Select" ]
        ]


viewCopyControl : RecordId -> Tab -> Html Msg
viewCopyControl recordId tab =
    let
        recordIdString =
            Record.idToString recordId

        arenaArg =
            WindowArena.initArgWithRecordId tab.tableName recordIdString

        copyArenaArg =
            { arenaArg | action = WindowArena.Copy recordIdString }

        iconColor =
            Constant.iconColor

        iconSize =
            Constant.rowControlIconSize
    in
    a
        [ Route.href (Route.WindowArena copyArenaArg)
        , onClick ClickedCopyRecord
        ]
        [ div [ class "duplicate-record tooltip" ]
            [ div [ ]
                [ MaterialContent.content_copy iconColor iconSize ]
            , span [ class "tooltip-text" ]
                [ text "Copy" ]
            ]
        ]


viewUndo : Model -> Html Msg
viewUndo model =
    let
        iconColor =
            Constant.iconColor

        iconSize =
            Constant.rowControlIconSize
    in
    div
        [ class "row-undo tooltip"
        , classList [ ( "is-active", isModified model ) ]
        , onClick ResetChanges
        ]
        [ div [ class "icon " ]
            [ MaterialContent.block iconColor iconSize ]
        , span [ class "tooltip-text" ]
            [ text "Undo" ]
        ]


viewSave : Model -> Html Msg
viewSave model =
    let
        iconColor =
            Constant.iconColor

        iconSize =
            Constant.rowControlIconSize
    in
    div
        [ class "row-save tooltip"
        , classList [ ( "is-active", isModified model ) ]
        ]
        [ div [ class "icon" ]
            [ MaterialContent.save iconColor iconSize ]
        , span [ class "tooltip-text" ]
            [ text "Save" ]
        ]


viewRecordDetail : RecordId -> Tab -> Html Msg
viewRecordDetail recordId tab =
    let
        recordIdString =
            Record.idToString recordId

        iconColor =
            Constant.iconColor

        iconSize =
            Constant.rowControlIconSize
    in
    a
        [ class "link-to-form tooltip"
        , onClick ClickedDetailedLink
        , Route.href (Route.WindowArena (WindowArena.initArgWithRecordId tab.tableName recordIdString))
        ]
        [ div [ class "icon" ]
            [ Ionicon.edit iconSize iconColor ]
        , span [ class "tooltip-text" ]
            [ text "Edit" ]
        ]


dropdownPageRequestNeeded : Model -> Maybe TableName
dropdownPageRequestNeeded model =
    List.filterMap
        (\value ->
            Field.dropdownPageRequestNeeded value
        )
        model.fields
        |> List.head


type Msg
    = FieldMsg Field.Model Field.Msg
    | AllFieldMsg Field.Msg
    | ResetChanges
    | ToggleSelect Bool
    | ClickedDetailedLink
    | ClickedCopyRecord
    | ContainerScrollChanged Scroll


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AllFieldMsg fieldMsg ->
            updateAllFields fieldMsg model

        FieldMsg argValue msg ->
            let
                updated =
                    List.map
                        (\value ->
                            if argValue == value then
                                let
                                    ( newValue, subCmd ) =
                                        Field.update msg value
                                in
                                ( newValue, Cmd.map (FieldMsg newValue) subCmd )
                            else
                                value => Cmd.none
                        )
                        model.fields

                ( updatedFields, subCmds ) =
                    List.unzip updated
            in
            { model | fields = updatedFields }
                => Cmd.batch subCmds

        ResetChanges ->
            let
                ( newFields, subCmds ) =
                    updateFields Field.ResetChanges model
                        |> List.unzip
            in
            { model | fields = newFields }
                => Cmd.batch
                    (List.map2
                        (\field cmd ->
                            Cmd.map (FieldMsg field) cmd
                        )
                        newFields
                        subCmds
                    )

        ToggleSelect v ->
            { model | selected = v }
                => Cmd.none

        -- handled in WindowArena
        ClickedDetailedLink ->
            model => Cmd.none

        -- handled in WindowArena
        ClickedCopyRecord ->
            model => Cmd.none

        ContainerScrollChanged scroll ->
            let
                updatedModel =
                    { model | containerScroll = scroll }
            in
            updateAllFields (Field.ContainerScrollChanged scroll) updatedModel


updateFields : Field.Msg -> Model -> List ( Field.Model, Cmd Field.Msg )
updateFields msg model =
    List.map (Field.update msg) model.fields


updateAllFields : Field.Msg -> Model -> ( Model, Cmd Msg )
updateAllFields fieldMsg model =
    let
        ( updatedFields, subCmd ) =
            List.map (Field.update fieldMsg) model.fields
                |> List.unzip
    in
    { model | fields = updatedFields }
        => Cmd.none
