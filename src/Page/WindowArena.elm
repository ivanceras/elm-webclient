port module Page.WindowArena
    exposing
        ( Model
        , Msg(..)
        , init
        , subscriptions
        , update
        , view
        )

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Constant
import Data.DatabaseName as DatabaseName exposing (DatabaseName)
import Data.Session as Session exposing (Session)
import Data.Window as Window exposing (Tag)
import Data.Window.Lookup as Lookup
import Data.Window.Presentation as Presentation exposing (Presentation(..))
import Data.Window.Record as Record exposing (RecordId)
import Data.Window.TableName as TableName exposing (TableName)
import Data.WindowArena as WindowArena exposing (Action(..), ArenaArg)
import Html exposing (..)
import Html.Attributes exposing (class, classList, id, style)
import Html.Events exposing (onClick)
import Http
import Ionicon
import Ionicon.Android as IoniconAndroid
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Request.Auth as Auth
import Request.Window
import Request.Window.Records
import Route
import SelectList exposing (SelectList)
import Settings exposing (Settings)
import String.Extra
import Task exposing (Task)
import Util exposing ((=>), styleIf, viewIf)
import Views.Page as Page
import Views.Window as Window
import Views.Window.DetailedRecord as DetailedRecord
import Views.Window.Field as Field
import Views.Window.GroupedWindow as GroupedWindow
import Views.Window.Row as Row
import Views.Window.Tab as Tab
import Views.Window.Toolbar as Toolbar
import Window as BrowserWindow exposing (Size)


-- MODEL --


type alias Model =
    { activeWindow : Maybe Window.Model
    , groupedWindow : GroupedWindow.Model
    , selectedRow : Maybe DetailedRecord.Model
    , arenaArg : ArenaArg
    , settings : Settings
    , errors : List String
    , loadingSelectedRecord : Bool
    , isDetailedRecordMaximized : Bool
    , containerSize : Size
    , isWindowListHidden : Bool
    }


handleLoadError e =
    pageLoadError Page.WindowArena ("WindowArena is currently unavailable. Error: " ++ toString e)


init : Settings -> Session -> ArenaArg -> Size -> Task PageLoadError Model
init settings session arenaArg containerSize =
    let
        _ =
            Debug.log "window arena: " arenaArg

        isDetailedRecordMaximized =
            Constant.isDetailedRecordMaximized

        maybeAuthToken =
            Maybe.map .token session.user

        tableName =
            arenaArg.tableName

        getDbName =
            Auth.dbName settings
                |> Task.mapError handleLoadError

        loadWindow =
            case tableName of
                Just tableName ->
                    Request.Window.get settings tableName
                        |> Http.toTask
                        |> Task.map Just
                        |> Task.mapError handleLoadError

                Nothing ->
                    Task.succeed Nothing

        loadActiveWindow =
            case tableName of
                Just tableName ->
                    Task.andThen
                        (\window ->
                            case window of
                                Just window ->
                                    Window.init settings session tableName window arenaArg (calcWindowSize containerSize settings.isWindowListHidden)
                                        |> Task.map Just
                                        |> Task.mapError handleLoadError

                                Nothing ->
                                    Task.succeed Nothing
                        )
                        loadWindow

                Nothing ->
                    Task.succeed Nothing

        loadWindowList =
            GroupedWindow.init settings tableName
                |> Task.mapError handleLoadError

        _ =
            Debug.log "action is:" arenaArg.action

        loadSelectedRecord =
            case tableName of
                Just tableName ->
                    Task.andThen
                        (\window ->
                            case window of
                                Just window ->
                                    case arenaArg.action of
                                        WindowArena.ListPage ->
                                            Task.succeed Nothing

                                        _ ->
                                            -- For Copy, Select, and New
                                            DetailedRecord.init isDetailedRecordMaximized settings tableName arenaArg.action arenaArg window (calcDetailedRecordSize containerSize settings.isWindowListHidden)
                                                |> Task.map Just
                                                |> Task.mapError handleLoadError

                                Nothing ->
                                    Task.succeed Nothing
                        )
                        loadWindow

                Nothing ->
                    Task.succeed Nothing
    in
    Task.map4
        (\activeWindow groupedWindow selectedRow dbName ->
            { activeWindow = activeWindow
            , groupedWindow = groupedWindow
            , selectedRow = selectedRow
            , arenaArg = arenaArg
            , settings = settings
            , errors = []
            , loadingSelectedRecord = False
            , isDetailedRecordMaximized = isDetailedRecordMaximized
            , containerSize = containerSize
            , isWindowListHidden = settings.isWindowListHidden
            }
        )
        loadActiveWindow
        loadWindowList
        loadSelectedRecord
        getDbName



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    let
        recordSelected =
            case model.selectedRow of
                Just _ ->
                    True

                Nothing ->
                    False

        showList =
            if recordSelected then
                not model.isDetailedRecordMaximized
            else
                True

        iconSize = 20
        iconColor = Constant.iconColor

        toggleIcon = 
            if model.isWindowListHidden then
                IoniconAndroid.menu  iconSize iconColor
            else
                IoniconAndroid.arrowBack iconSize iconColor
    in
    div [ class "window" ]
        [ viewBanner model
        , button [ class "sidebar-control"
                , onClick (ToggleWindowList (not model.isWindowListHidden)) ] 
                [ toggleIcon ]
        , div [ class "window-content" ]
            [ div [ class "pane-group" ]
                [ GroupedWindow.view model.groupedWindow
                    |> Html.map GroupedWindowMsg
                , div [ class "pane window-arena" ]
                    [ div [ class "tab-names" ]
                        [ viewTabNames model ]
                    , div [ class "window-and-selected-row" ]
                        [ viewWindow session model.activeWindow
                            |> viewIf showList
                        , viewSelectedRow session model
                        ]
                    ]
                ]
            ]
        ]


viewSelectedRow : Session -> Model -> Html Msg
viewSelectedRow session model =
    case model.selectedRow of
        Just selectedRow ->
            DetailedRecord.view
                selectedRow
                |> Html.map DetailedRecordMsg

        Nothing ->
            text ""


viewWindowOrSelectedRow : Session -> Model -> Html Msg
viewWindowOrSelectedRow session model =
    case model.selectedRow of
        Just selectedRow ->
            DetailedRecord.view selectedRow
                |> Html.map DetailedRecordMsg

        Nothing ->
            viewWindow session model.activeWindow


viewWindow : Session -> Maybe Window.Model -> Html Msg
viewWindow session activeWindow =
    case activeWindow of
        Just activeWindow ->
            div [ class "window-view" ]
                [ Window.view
                    session
                    activeWindow
                    |> Html.map WindowMsg
                ]

        Nothing ->
            text "No active window"


viewTabNames : Model -> Html msg
viewTabNames model =
    let
        inDetail =
            Util.isJust model.selectedRow
    in
    case model.activeWindow of
        Just activeWindow ->
            a
                [ class "tab-name active-main-tab"
                , classList [ ( "in-selected-record", inDetail ) ]
                , Route.href (Route.WindowArena model.arenaArg)
                ]
                [ text activeWindow.mainTab.tab.name ]

        Nothing ->
            text "no tab"


viewBanner : Model -> Html Msg
viewBanner model =
    let
        ( dbName, dbDescription ) =
            case model.settings.dbName of
                Just dbName ->
                    let
                        db =
                            String.Extra.toTitleCase dbName.name
                    in
                    case dbName.description of
                        Just desc ->
                            ( db, desc )

                        Nothing ->
                            ( db, "Powered by Diwata - a user friendly database interface" )

                Nothing ->
                    ( "Diwata", "a user-friendly database interface" )
    in
    div
        [ class "banner"
        , id "banner"
        , classList [ ( "hide", model.isWindowListHidden ) ]
        ]
        [ div [ class "head" ]
            [ h3 [ class "logo-font" ] [ text dbName ]
            , text dbDescription
            ]
        , viewLoadingIndicator model
        ]


viewLoadingIndicator : Model -> Html Msg
viewLoadingIndicator model =
    let
        iconColor =
            Constant.iconColor

        iconSize =
            30
    in
    div
        [ class "selected-record-loading-indicator spin animated fadeIn"
        ]
        [ Ionicon.loadA iconSize iconColor ]
        |> viewIf model.loadingSelectedRecord



-- UPDATE --


type Msg
    = GroupedWindowMsg GroupedWindow.Msg
    | WindowMsg Window.Msg
    | DetailedRecordMsg DetailedRecord.Msg
    | BrowserResized Size
    | InitializedSelectedRow ( DetailedRecord.Model, Maybe RecordId )
    | FailedToInitializeSelectedRow
    | ToggleWindowList Bool
    | SetSettings Settings


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    let
        isDetailedRecordMaximized =
            model.isDetailedRecordMaximized

        arenaArg =
            model.arenaArg
    in
    case msg of
        GroupedWindowMsg subMsg ->
            let
                ( newFeed, subCmd ) =
                    GroupedWindow.update subMsg model.groupedWindow
            in
            { model | groupedWindow = newFeed } => Cmd.map GroupedWindowMsg subCmd

        WindowMsg (Window.TabMsg (Tab.RowMsg rowModel Row.ClickedCopyRecord)) ->
            let
                recordIdString =
                    Record.idToString rowModel.recordId

                tableName =
                    rowModel.tab.tableName

                activeWindow =
                    case model.activeWindow of
                        Just activeWindow ->
                            activeWindow.window

                        Nothing ->
                            Debug.crash "There should be an activeWindow"

                copyArenaArg =
                    { arenaArg | action = Copy recordIdString }

                initSelectedRow =
                    DetailedRecord.init isDetailedRecordMaximized model.settings tableName (Copy recordIdString) copyArenaArg activeWindow (calcDetailedRecordSize model.containerSize model.isWindowListHidden)

                initSelectedRowTask =
                    Task.attempt
                        (\result ->
                            case result of
                                Ok result ->
                                    InitializedSelectedRow ( result, Just rowModel.recordId )

                                Err e ->
                                    FailedToInitializeSelectedRow
                        )
                        initSelectedRow
            in
            { model | loadingSelectedRecord = True }
                => initSelectedRowTask

        WindowMsg (Window.TabMsg (Tab.RowMsg rowModel Row.ClickedDetailedLink)) ->
            let
                recordIdString =
                    Record.idToString rowModel.recordId

                tableName =
                    rowModel.tab.tableName

                activeWindow =
                    case model.activeWindow of
                        Just activeWindow ->
                            activeWindow.window

                        Nothing ->
                            Debug.crash "There should be an activeWindow"

                initSelectedRow =
                    DetailedRecord.init isDetailedRecordMaximized model.settings tableName arenaArg.action arenaArg activeWindow (calcDetailedRecordSize model.containerSize model.isWindowListHidden)

                initSelectedRowTask =
                    Task.attempt
                        (\result ->
                            case result of
                                Ok result ->
                                    InitializedSelectedRow ( result, Just rowModel.recordId )

                                Err e ->
                                    FailedToInitializeSelectedRow
                        )
                        initSelectedRow
            in
            { model | loadingSelectedRecord = True }
                => initSelectedRowTask

        WindowMsg (Window.TabMsg (Tab.RowMsg rowModel (Row.FieldMsg fieldModel (Field.PrimaryLinkClicked tableName recordIdString)))) ->
            let
                activeWindow =
                    case model.activeWindow of
                        Just activeWindow ->
                            activeWindow.window

                        Nothing ->
                            Debug.crash "There should be an activeWindow"

                initSelectedRow =
                    DetailedRecord.init isDetailedRecordMaximized model.settings tableName arenaArg.action arenaArg activeWindow (calcDetailedRecordSize model.containerSize model.isWindowListHidden)

                initSelectedRowTask =
                    Task.attempt
                        (\result ->
                            case result of
                                Ok result ->
                                    InitializedSelectedRow ( result, Just rowModel.recordId )

                                Err e ->
                                    FailedToInitializeSelectedRow
                        )
                        initSelectedRow
            in
            { model | loadingSelectedRecord = True }
                => initSelectedRowTask

        InitializedSelectedRow ( selectedRow, recordId ) ->
            { model
                | selectedRow = Just selectedRow
                , loadingSelectedRecord = False
            }
                => Cmd.none

        FailedToInitializeSelectedRow ->
            { model
                | errors = "Failed to initialize selected row" :: model.errors
                , loadingSelectedRecord = False
            }
                => Cmd.none

        WindowMsg (Window.TabMsg (Tab.ToolbarMsg Toolbar.ClickedNewButton)) ->
            let
                activeWindow =
                    case model.activeWindow of
                        Just activeWindow ->
                            activeWindow.window

                        Nothing ->
                            Debug.crash "There should be an activeWindow"

                tableName =
                    case arenaArg.tableName of
                        Just tableName ->
                            tableName

                        Nothing ->
                            Debug.crash "There should be tableName"

                newArenaArg =
                    { arenaArg | action = NewRecord InCard }

                initSelectedRow =
                    DetailedRecord.init isDetailedRecordMaximized model.settings tableName (NewRecord InCard) newArenaArg activeWindow (calcDetailedRecordSize model.containerSize model.isWindowListHidden)

                initNewRecordTask =
                    Task.attempt
                        (\result ->
                            case result of
                                Ok result ->
                                    InitializedSelectedRow ( result, Nothing )

                                Err e ->
                                    FailedToInitializeSelectedRow
                        )
                        initSelectedRow
            in
            { model
                | loadingSelectedRecord = True
                , arenaArg = newArenaArg
            }
                => Cmd.batch
                    [ initNewRecordTask
                    , Route.modifyUrl (Route.WindowArena newArenaArg)
                    ]

        WindowMsg subMsg ->
            case model.activeWindow of
                Just activeWindow ->
                    let
                        lookup =
                            activeWindow.lookup

                        ( newWindow, subCmd ) =
                            Window.update subMsg activeWindow

                        ( updatedWindow, windowCmd ) =
                            case Window.dropdownPageRequestNeeded activeWindow of
                                Just sourceTable ->
                                    let
                                        ( currentPage, listRecord ) =
                                            Lookup.tableLookup sourceTable lookup
                                    in
                                    { newWindow | dropdownPageRequestInFlight = True }
                                        => requestNextDropdownPageForWindow model.settings currentPage sourceTable

                                Nothing ->
                                    newWindow => Cmd.none
                    in
                    { model | activeWindow = Just updatedWindow }
                        => Cmd.batch
                            [ Cmd.map WindowMsg subCmd
                            , windowCmd
                            ]

                Nothing ->
                    model => Cmd.none

        DetailedRecordMsg (DetailedRecord.ToolbarMsg Toolbar.ClickedClose) ->
            closeRecord model

        DetailedRecordMsg DetailedRecord.ClickedCloseButton ->
            closeRecord model

        DetailedRecordMsg (DetailedRecord.ToolbarMsg (Toolbar.ClickedMaximize v)) ->
            let
                ( updatedSelectedRow, cmd ) =
                    case model.selectedRow of
                        Just selectedRow ->
                            let
                                ( detailedRecord, subCmd ) =
                                    DetailedRecord.update (DetailedRecord.Maximize v) selectedRow
                            in
                            ( Just detailedRecord, Cmd.map DetailedRecordMsg subCmd )

                        Nothing ->
                            ( Nothing, Cmd.none )
            in
            { model
                | isDetailedRecordMaximized = v
                , selectedRow = updatedSelectedRow
            }
                => cmd

        DetailedRecordMsg subMsg ->
            case model.selectedRow of
                Just selectedRow ->
                    let
                        lookup =
                            selectedRow.lookup

                        ( newDetailedRecord, subCmd ) =
                            DetailedRecord.update subMsg selectedRow

                        ( updatedDetailedRecord, detailCmd ) =
                            case DetailedRecord.dropdownPageRequestNeeded selectedRow of
                                Just sourceTable ->
                                    let
                                        ( currentPage, listRecord ) =
                                            Lookup.tableLookup sourceTable lookup
                                    in
                                    { newDetailedRecord | dropdownPageRequestInFlight = True }
                                        => requestNextDropdownPageForDetailedRecord model.settings currentPage sourceTable

                                Nothing ->
                                    newDetailedRecord => Cmd.none
                    in
                    { model | selectedRow = Just updatedDetailedRecord }
                        => Cmd.batch
                            [ Cmd.map DetailedRecordMsg subCmd
                            , detailCmd
                            ]

                Nothing ->
                    model => Cmd.none

        BrowserResized size ->
            let
                _ =
                    Debug.log "updating containerSize in windowArena" size

                updatedModel =
                    { model | containerSize = size }

                ( updatedModel2, subCmd2 ) =
                    updateActiveWindowSize updatedModel

                ( updatedModel3, subCmd3 ) =
                    updateDetailedRecordSize updatedModel2
            in
            updatedModel3 => Cmd.batch [ subCmd2, subCmd2 ]

        ToggleWindowList isHidden ->
            let
                updatedModel =
                    { model | isWindowListHidden = isHidden }

                groupedWindow =
                    model.groupedWindow

                ( updatedGroupedWindow, _ ) =
                    GroupedWindow.update (GroupedWindow.SetVisibility isHidden) groupedWindow

                updatedModel2 =
                    { updatedModel | groupedWindow = updatedGroupedWindow }

                ( updatedModel3, subCmd3 ) =
                    updateActiveWindowSize updatedModel2

                ( updatedModel4, subCmd4 ) =
                    updateDetailedRecordSize updatedModel3
            in
            updatedModel4 => Cmd.batch [ subCmd3, subCmd4 ]

        SetSettings settings ->
            let
                _ =
                    Debug.log "window arena settings " settings
            in
            { model | settings = settings }
                => Cmd.none


updateDetailedRecordSize : Model -> ( Model, Cmd Msg )
updateDetailedRecordSize model =
    updateSelectedRow (DetailedRecord.ContainerSizeChanged (calcDetailedRecordSize model.containerSize model.isWindowListHidden)) model


updateSelectedRow : DetailedRecord.Msg -> Model -> ( Model, Cmd Msg )
updateSelectedRow detailMsg model =
    let
        ( selectedRow, subCmd ) =
            case model.selectedRow of
                Just selectedRow ->
                    DetailedRecord.update detailMsg selectedRow
                        |> Tuple.mapFirst Just
                        |> Tuple.mapSecond (Cmd.map DetailedRecordMsg)

                Nothing ->
                    ( model.selectedRow, Cmd.none )
    in
    { model | selectedRow = selectedRow }
        => subCmd


calcWindowSize : Size -> Bool -> Size
calcWindowSize containerSize isWindowListHidden =
    let
        bannerHeightDeduction =
            if isWindowListHidden then
                0
            else
                Constant.bannerHeight

        heightDeductions =
            bannerHeightDeduction + Constant.tabNameHeight

        widthDeductions =
            if isWindowListHidden then
                0
            else
                Constant.sidebarWidth
    in
    { width = containerSize.width - widthDeductions
    , height = containerSize.height - heightDeductions
    }


calcDetailedRecordSize : Size -> Bool -> Size
calcDetailedRecordSize containerSize isWindowListHidden =
    calcWindowSize containerSize isWindowListHidden


updateActiveWindowSize : Model -> ( Model, Cmd Msg )
updateActiveWindowSize model =
    let
        windowSize =
            calcWindowSize model.containerSize model.isWindowListHidden
    in
    updateActiveWindow (Window.ContainerSizeChanged windowSize) model


updateActiveWindow : Window.Msg -> Model -> ( Model, Cmd Msg )
updateActiveWindow windowMsg model =
    let
        ( activeWindow, subCmd ) =
            case model.activeWindow of
                Just activeWindow ->
                    Window.update windowMsg activeWindow
                        |> Tuple.mapFirst Just
                        |> Tuple.mapSecond (Cmd.map WindowMsg)

                Nothing ->
                    ( model.activeWindow, Cmd.none )
    in
    { model | activeWindow = activeWindow }
        => subCmd


closeRecord : Model -> ( Model, Cmd Msg )
closeRecord model =
    let
        updatedArenaArg =
            WindowArena.removeSelected model.arenaArg
    in
    { model
        | selectedRow = Nothing
        , arenaArg = updatedArenaArg
    }
        => Route.modifyUrl (Route.WindowArena updatedArenaArg)


requestNextDropdownPageForWindow : Settings -> Int -> TableName -> Cmd Msg
requestNextDropdownPageForWindow settings currentPage sourceTable =
    Request.Window.Records.lookupPage settings (currentPage + 1) Nothing sourceTable
        |> Http.toTask
        |> Task.attempt
            (\result ->
                case result of
                    Ok rows ->
                        let
                            recordList =
                                Record.rowsToRecordList rows
                        in
                        WindowMsg (Window.LookupNextPageReceived ( sourceTable, recordList ))

                    Err e ->
                        WindowMsg (Window.LookupNextPageErrored (toString e))
            )


requestNextDropdownPageForDetailedRecord : Settings -> Int -> TableName -> Cmd Msg
requestNextDropdownPageForDetailedRecord settings currentPage sourceTable =
    Request.Window.Records.lookupPage settings (currentPage + 1) Nothing sourceTable
        |> Http.toTask
        |> Task.attempt
            (\result ->
                case result of
                    Ok rows ->
                        let
                            recordList =
                                Record.rowsToRecordList rows
                        in
                        DetailedRecordMsg (DetailedRecord.LookupNextPageReceived ( sourceTable, recordList ))

                    Err e ->
                        DetailedRecordMsg (DetailedRecord.LookupNextPageErrored (toString e))
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ detailedRecordSubscriptions model
        , windowSubscriptions model
        ]


detailedRecordSubscriptions : Model -> Sub Msg
detailedRecordSubscriptions model =
    case model.selectedRow of
        Just selectedRow ->
            Sub.map DetailedRecordMsg (DetailedRecord.subscriptions selectedRow)

        Nothing ->
            Sub.none


windowSubscriptions : Model -> Sub Msg
windowSubscriptions model =
    case model.activeWindow of
        Just activeWindow ->
            Sub.map WindowMsg (Window.subscriptions activeWindow)

        Nothing ->
            Sub.none
