module Views.Window.DetailedRecord
    exposing
        ( Model
        , Msg(..)
        , dropdownPageRequestNeeded
        , init
        , subscriptions
        , update
        , view
        )

import Constant
import Data.DataContainer as DataContainer exposing (RecordDetailChangeset, RecordLinkAction(..))
import Data.Query as Query
import Data.Query.Sort as Sort exposing (Sort)
import Data.Session as Session exposing (Session)
import Data.Window as Window exposing (Window)
import Data.Window.Field as Field exposing (Field, FieldContainer(..))
import Data.Window.Lookup as Lookup exposing (Lookup)
import Data.Window.Presentation as Presentation exposing (Presentation(..))
import Data.Window.Record as Record exposing (Record, Rows)
import Data.Window.RecordDetail as RecordDetail exposing (RecordDetail)
import Data.Window.Tab as Tab exposing (Tab, TabType(..))
import Data.Window.TableName as TableName exposing (TableName)
import Data.Window.Value as Value exposing (Value)
import Data.WindowArena as WindowArena exposing (Action(..), ArenaArg, Section(..))
import Dict
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, draggable, href, id, placeholder, src, style)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Mouse exposing (Position)
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Request.Window
import Request.Window.DataUpdate as DataUpdate
import Request.Window.Records as Records
import Route
import Settings exposing (Settings)
import Task exposing (Task)
import Util exposing ((=>), Scroll, onClickPreventDefault, onScroll, px, styleIf, viewIf)
import Views.Page as Page
import Views.Window as Window
import Views.Window.Field as Field
import Views.Window.Row as Row
import Views.Window.Tab as Tab
import Views.Window.Toolbar as Toolbar
import Window as BrowserWindow exposing (Size)


{-| Example:
<http://localhost:8000/#/window/bazaar.product/select/f7521093-734d-488a-9f60-fc9f11f7e750>
-}



-- MODEL


type alias Model =
    { selectedRow : Maybe RecordDetail
    , window : Window
    , hasManyTabs : List Tab.Model
    , indirectTabs : List ( TableName, Tab.Model )
    , position : Position
    , drag : Maybe DragPosition
    , scroll : Scroll
    , containerSize : Size
    , arenaArg : ArenaArg
    , lookup : Lookup
    , values : List Field.Model
    , oneOneValues : List ( Tab, List Field.Model )
    , dropdownPageRequestInFlight : Bool
    , settings : Settings
    , isMaximized : Bool
    , errors : List String
    }


type alias DragPosition =
    { start : Position
    , current : Position
    }


init : Bool -> Settings -> TableName -> Action -> ArenaArg -> Window -> Size -> Task PageLoadError Model
init isMaximized settings tableName action arenaArg window containerSize =
    let
        doFetchSelected recordId =
            Records.fetchSelected settings tableName recordId
                |> Http.toTask
                |> Task.mapError handleLoadError
                |> Task.map Just

        fetchSelected =
            case action of
                WindowArena.Select recordId ->
                    doFetchSelected recordId

                WindowArena.Copy recordId ->
                    doFetchSelected recordId

                _ ->
                    Task.succeed Nothing

        loadWindowLookups =
            Records.lookups settings Nothing tableName
                |> Http.toTask
                |> Task.mapError handleLoadError

        splitPercentage =
            case arenaArg.sectionSplit of
                Just split ->
                    split

                Nothing ->
                    0.6

        sectionQuery =
            arenaArg.sectionQuery

        initHasManyTabs =
            Task.map2
                (\detailRows lookup ->
                    let
                        ( mainRecordHeight, detailTabHeight ) =
                            splitTabHeights window (initialPosition splitPercentage isMaximized containerSize) isMaximized containerSize

                        ( allotedWidth, allotedHeight ) =
                            allotedSize isMaximized containerSize

                        tabSize =
                            ( allotedWidth, detailTabHeight )
                    in
                    List.map
                        (\hasManyTab ->
                            let
                                rows =
                                    case detailRows of
                                        Just detailRows ->
                                            case action of
                                                Select _ ->
                                                    RecordDetail.contentInTable detailRows.hasMany hasManyTab.tableName

                                                _ ->
                                                    Just Record.emptyRow

                                        Nothing ->
                                            Just Record.emptyRow
                            in
                            case rows of
                                Just rows ->
                                    Tab.init arenaArg settings Nothing tabSize sectionQuery hasManyTab InHasMany rows lookup

                                Nothing ->
                                    Debug.crash "Empty row"
                        )
                        window.hasManyTabs
                )
                fetchSelected
                loadWindowLookups

        initIndirectTabs =
            Task.map2
                (\detailRows lookup ->
                    let
                        ( mainRecordHeight, detailTabHeight ) =
                            splitTabHeights window (initialPosition splitPercentage isMaximized containerSize) isMaximized containerSize

                        ( allotedWidth, allotedHeight ) =
                            allotedSize isMaximized containerSize

                        tabSize =
                            ( allotedWidth, detailTabHeight )
                    in
                    List.map
                        (\( linker, indirectTab ) ->
                            let
                                rows =
                                    case detailRows of
                                        Just detailRows ->
                                            case action of
                                                Select _ ->
                                                    RecordDetail.contentInIndirectTable detailRows.indirect linker indirectTab.tableName

                                                _ ->
                                                    Just Record.emptyRow

                                        Nothing ->
                                            Just Record.emptyRow
                            in
                            case rows of
                                Just rows ->
                                    ( linker, Tab.init arenaArg settings Nothing tabSize sectionQuery indirectTab InIndirect rows lookup )

                                Nothing ->
                                    Debug.crash "Empty row"
                        )
                        window.indirectTabs
                )
                fetchSelected
                loadWindowLookups
    in
    Task.map4
        (\detail hasManyTabs indirectTabs lookup ->
            let
                action =
                    arenaArg.action

                ( allotedWidth, allotedHeight ) =
                    allotedSize isMaximized containerSize

                allotedTabWidth =
                    round allotedWidth
            in
            { selectedRow = detail
            , window = window
            , hasManyTabs = hasManyTabs
            , indirectTabs = indirectTabs
            , position = initialPosition splitPercentage isMaximized containerSize
            , drag = Nothing
            , scroll = Scroll 0 0
            , containerSize = containerSize
            , arenaArg = arenaArg
            , lookup = lookup
            , values =
                case action of
                    NewRecord presentation ->
                        createFields allotedTabWidth (NewRecord presentation) window.mainTab lookup Nothing

                    Select _ ->
                        createFields allotedTabWidth action window.mainTab lookup (Maybe.map .record detail)

                    Copy _ ->
                        createFields allotedTabWidth action window.mainTab lookup (Maybe.map .record detail)

                    ListPage ->
                        []
            , oneOneValues =
                case detail of
                    Just detail ->
                        createOneOneFields allotedTabWidth action window.oneOneTabs lookup detail.oneOnes

                    Nothing ->
                        []
            , dropdownPageRequestInFlight = False
            , settings = settings
            , isMaximized = isMaximized
            , errors = []
            }
        )
        fetchSelected
        initHasManyTabs
        initIndirectTabs
        loadWindowLookups


{-|

    Get the changeset of this record including the
    oneOnes
    hasMany (updated, unlinked, linkedExisting, linkedNew )
    indirect ( updated, unlinked, linkedExisting, linkedNew )

-}
getChangeset : Model -> RecordDetailChangeset
getChangeset model =
    let
        arenaArg =
            model.arenaArg

        action =
            arenaArg.action

        recordAction =
            case action of
                ListPage ->
                    Debug.crash "unexpected action"

                Select s ->
                    Edited

                NewRecord p ->
                    CreateNew

                Copy s ->
                    CreateNew
    in
    { record = editedRecord model
    , action = recordAction
    , oneOnes = getOneOneRecord model
    , hasMany =
        getHasManyUpdatedRows model
            ++ getHasManyLinkNewRows model
            ++ getHasManyUnlinkedRows model
    , indirect =
        getIndirectLinkNewRows model
            ++ getIndirectLinkExistingRows model
            ++ getIndirectUnlinkedRows model
    }


{-|

    The edited record, collection of edited value for each field

-}
editedRecord : Model -> Record
editedRecord model =
    List.map
        (\field ->
            let
                editedValue =
                    Field.editedValue field

                columnName =
                    Field.columnName field.field
            in
            ( columnName, editedValue )
        )
        model.values
        |> Dict.fromList


{-|

    Get one one record for each oneOneTab

-}
getOneOneRecord : Model -> List ( TableName, Maybe Record )
getOneOneRecord model =
    List.map
        (\( oneOneTab, fields ) ->
            let
                record =
                    List.map
                        (\field ->
                            let
                                editedValue =
                                    Field.editedValue field

                                columnName =
                                    Field.columnName field.field
                            in
                            ( columnName, editedValue )
                        )
                        fields
                        |> Dict.fromList
                        |> Maybe.Just
            in
            ( oneOneTab.tableName, record )
        )
        model.oneOneValues


{-|

    return the updated rows for the each hasmany tab
    Note: indirect tab is not allowed to edit any of the row,
        If you want to edit the indirect record, click on it and edit it on its own container (DetailedRecord)

-}
getHasManyUpdatedRows : Model -> List ( TableName, RecordLinkAction, Rows )
getHasManyUpdatedRows model =
    List.map
        (\hasMany ->
            let
                hasManyTab =
                    hasMany.tab

                updatedRows =
                    Tab.editedRows hasMany
            in
            ( hasManyTab.tableName, Edited, updatedRows )
        )
        model.hasManyTabs


{-|

    get the linked new rows for each of the hasMany tables

-}
getHasManyLinkNewRows : Model -> List ( TableName, RecordLinkAction, Rows )
getHasManyLinkNewRows model =
    List.map
        (\hasMany ->
            let
                linkNewRows =
                    Tab.getLinkNewRows hasMany
            in
            ( hasMany.tab.tableName, LinkNew, linkNewRows )
        )
        model.hasManyTabs


{-|

    get the linked new rows of the indirect tables

-}
getIndirectLinkNewRows : Model -> List ( TableName, TableName, RecordLinkAction, Rows )
getIndirectLinkNewRows model =
    List.map
        (\( viaTableName, indirect ) ->
            let
                updatedRows =
                    Tab.getLinkNewRows indirect
            in
            ( indirect.tab.tableName, viaTableName, LinkNew, updatedRows )
        )
        model.indirectTabs


getIndirectLinkExistingRows : Model -> List ( TableName, TableName, RecordLinkAction, Rows )
getIndirectLinkExistingRows model =
    List.map
        (\( viaTableName, indirect ) ->
            let
                updatedRows =
                    Tab.getLinkExistingRows indirect
            in
            ( indirect.tab.tableName, viaTableName, LinkExisting, updatedRows )
        )
        model.indirectTabs


getIndirectUnlinkedRows : Model -> List ( TableName, TableName, RecordLinkAction, Rows )
getIndirectUnlinkedRows model =
    List.map
        (\( viaTableName, indirect ) ->
            let
                unlinkedRows =
                    Tab.getUnlinkedRows indirect
            in
            ( indirect.tab.tableName, viaTableName, Unlink, unlinkedRows )
        )
        model.indirectTabs


getHasManyUnlinkedRows : Model -> List ( TableName, RecordLinkAction, Rows )
getHasManyUnlinkedRows model =
    List.map
        (\hasMany ->
            let
                unlinkedRows =
                    Tab.getUnlinkedRows hasMany
            in
            ( hasMany.tab.tableName, Unlink, unlinkedRows )
        )
        model.hasManyTabs


initialPosition : Float -> Bool -> BrowserWindow.Size -> Position
initialPosition split isMaximized containerSize =
    let
        ( allotedWidth, allotedHeight ) =
            allotedSize isMaximized containerSize

        allotedMainHeight =
            round (allotedHeight * split)

        -- 60% main tab, 40% detail tabs
    in
    Position 0 allotedMainHeight


splitPercentage : Model -> Float
splitPercentage model =
    let
        ( allotedWidth, allotedHeight ) =
            detailAllotedSize model

        dragPosition =
            clamp 0 allotedHeight (toFloat model.position.y)
    in
    dragPosition / allotedHeight


{-|

    Check if the any of values of the detail records is modified.
    This includes the records on the detail and the one one record linked

-}
isModified : Model -> Bool
isModified model =
    let
        detailModified =
            List.any Field.isModified model.values

        oneOneModified =
            List.any
                (\( tab, fields ) ->
                    List.any Field.isModified fields
                )
                model.oneOneValues

        hasManyModified =
            List.any
                (\tab ->
                    Tab.isModified tab
                )
                model.hasManyTabs

        indirectModified =
            List.any
                (\( via, tab ) ->
                    Tab.isModified tab
                )
                model.indirectTabs
    in
    detailModified
        || oneOneModified
        || hasManyModified
        || indirectModified


handleLoadError : Http.Error -> PageLoadError
handleLoadError e =
    pageLoadError Page.WindowArena ("WindowArena DetailedRecord is currently unavailable. Error: " ++ toString e)


createOneOneFields : Int -> Action -> List Tab -> Lookup -> List ( TableName, Maybe Record ) -> List ( Tab, List Field.Model )
createOneOneFields allotedTabWidth action oneOneTabs lookup oneOneRecords =
    List.map
        (\( tableName, record ) ->
            let
                oneTab =
                    List.filter
                        (\tab ->
                            tab.tableName == tableName
                        )
                        oneOneTabs
                        |> List.head
            in
            case oneTab of
                Just oneTab ->
                    ( oneTab, createFields allotedTabWidth action oneTab lookup record )

                Nothing ->
                    Debug.crash "There should be a oneTab"
        )
        oneOneRecords


dropdownPageRequestNeeded : Model -> Maybe TableName
dropdownPageRequestNeeded model =
    let
        mainFields =
            List.filterMap
                (\value ->
                    Field.dropdownPageRequestNeeded value
                )
                model.values

        hasManyTabFields =
            List.filterMap
                (\hasManyTab ->
                    Tab.dropdownPageRequestNeeded hasManyTab
                )
                model.hasManyTabs

        indirectTabFields =
            List.filterMap
                (\( linker, indirectTab ) ->
                    Tab.dropdownPageRequestNeeded indirectTab
                )
                model.indirectTabs

        sourceTable =
            mainFields
                ++ hasManyTabFields
                ++ indirectTabFields
                |> List.head
    in
    if not model.dropdownPageRequestInFlight then
        sourceTable
    else
        Nothing


createFields : Int -> Action -> Tab -> Lookup -> Maybe Record -> List Field.Model
createFields allotedTabWidth action tab lookup record =
    List.map
        (\field ->
            Field.init allotedTabWidth InCard action record tab lookup field
        )
        tab.fields


detailAllotedSize : Model -> ( Float, Float )
detailAllotedSize model =
    allotedSize model.isMaximized model.containerSize


allotedSize : Bool -> BrowserWindow.Size -> ( Float, Float )
allotedSize isMaximized containerSize =
    let
        ( width, height ) =
            ( toFloat containerSize.width, toFloat containerSize.height )

        sideMargins =
            60

        fixMarginBottom =
            60

        marginBottom =
            if isMaximized then
                fixMarginBottom + 0
            else
                fixMarginBottom + 40

        totalWidthDeductions =
            if isMaximized then
                sideMargins
            else
                Constant.detailedMarginLeft + sideMargins

        totalHeightDeductions =
            marginBottom
    in
    ( width - totalWidthDeductions, height - totalHeightDeductions )


{-| Split tab heights (MainRecordHeight, DetailRecordHeight)
-}
splitTabHeights : Window -> Position -> Bool -> BrowserWindow.Size -> ( Float, Float )
splitTabHeights window position isMaximized containerSize =
    let
        cardToolbar =
            60

        detailToolbar =
            60

        detailTabNamesHeight =
            40

        detailColumnHeights =
            70

        separatorHeight =
            10

        margins =
            0

        cardTotalDeductions =
            cardToolbar + margins

        detailTotalDeductions =
            detailToolbar + detailTabNamesHeight + separatorHeight + detailColumnHeights

        ( width, height ) =
            allotedSize isMaximized containerSize

        allotedHeight =
            height - cardTotalDeductions

        mainRecordHeight =
            toFloat position.y

        clampMainRecordHeight =
            clamp 0 (allotedHeight - detailTotalDeductions) mainRecordHeight

        detailRecordHeight =
            allotedHeight - clampMainRecordHeight - detailTotalDeductions

        clampDetailRecordHeight =
            clamp 0 (allotedHeight - detailTotalDeductions) detailRecordHeight
    in
    ( clampMainRecordHeight, clampDetailRecordHeight )


view : Model -> Html Msg
view model =
    let
        window =
            model.window

        mainTab =
            window.mainTab

        realPosition =
            getPosition model

        isMaximized =
            model.isMaximized || Constant.isDetailedRecordMaximized

        containerSize =
            model.containerSize

        ( mainRecordHeight, detailTabHeight ) =
            splitTabHeights window realPosition isMaximized containerSize

        ( allotedWidth, allotedHeight ) =
            detailAllotedSize model

        -- TODO: this is HACKY, maybe refactor the toolbar for each specific use case such as the detail record
        -- away from the main tab use case
        toolbarModel =
            { selected = 0
            , modified =
                if isModified model then
                    1
                else
                    0
            , showIconText = allotedWidth > Constant.showIconTextMinWidth
            , moveDownIconText =
                allotedWidth
                    < Constant.moveDownIconTextMinWidth
                    && allotedWidth
                    > Constant.showIconTextMinWidth
            , multiColumnSort = False
            }

        containerHeight =
            allotedHeight + 40

        containerWidth =
            allotedWidth + 50
    in
    div
        [ class "detailed-selected-row animated fadeInDown"
        , style
            [ ( "height", px containerHeight )
            , ( "width", px containerWidth )
            ]
        , Constant.detailedSelectedRowStyle
            |> styleIf (not isMaximized)

        --shadow only if record is not maximized
        , classList [ ( "detailed-selected-row--shadow", not isMaximized ) ]
        ]
        [ div
            [ class "toolbar-area" ]
            [ div
                [ class "detail-record-window-cmd-buttons" ]
                [ div
                    [ class "window-cmd-close"
                    , onClick ClickedCloseButton
                    ]
                    [ i [ class "fa fa-times-circle-o fa-2x" ] [] ]
                ]
            , Toolbar.viewForDetailRecord toolbarModel
                |> Html.map ToolbarMsg
            ]
        , div
            [ class "main-tab-selected"
            , style [ ( "height", px mainRecordHeight ) ]
            , onScroll ViewScrolled
            ]
            [ cardViewRecord Detail model.values mainTab model
            , viewOneOneTabs model
            ]
        , viewIf (Window.hasDetails model.window)
            (div
                [ class "detail-tabs-with-separator"
                ]
                [ div
                    [ onMouseDown
                    , class "detail-separator"
                    ]
                    [ i
                        [ class "icon icon-dot-3"
                        ]
                        []
                    ]
                , viewDetailTabs model
                ]
            )
        ]


viewOneOneTabs : Model -> Html Msg
viewOneOneTabs model =
    let
        window =
            model.window

        selectedRow =
            model.selectedRow

        oneOneValues =
            model.oneOneValues
    in
    div []
        (List.map
            (\( oneOneTab, values ) ->
                oneOneCardView values oneOneTab model
            )
            oneOneValues
        )


oneOneCardView : List Field.Model -> Tab -> Model -> Html Msg
oneOneCardView oneOneValues oneOneTab model =
    let
        ( allotedWidth, allotedHeight ) =
            detailAllotedSize model

        cardWidth =
            allotedWidth - 100
    in
    div
        [ class "one-one-tab"
        , style [ ( "width", px cardWidth ) ]
        ]
        [ div [ class "one-one-tab-separator" ] [ text oneOneTab.name ]
        , cardViewRecord OneOne oneOneValues oneOneTab model
        ]


cardViewRecord : FieldContainer -> List Field.Model -> Tab -> Model -> Html Msg
cardViewRecord container values tab model =
    let
        lookup =
            model.lookup

        columnNames =
            Tab.columnNames tab

        maxColumnLen =
            List.map String.length columnNames
                |> List.maximum

        fieldLabelWidth =
            case maxColumnLen of
                Just len ->
                    len * 12

                Nothing ->
                    200

        isMaximized =
            model.isMaximized

        containerSize =
            model.containerSize

        ( allotedWidth, allotedHeight ) =
            detailAllotedSize model

        cardWidth =
            case container of
                OneOne ->
                    allotedWidth - 100

                Detail ->
                    allotedWidth
    in
    div []
        [ div
            [ class "card-view"
            , style [ ( "width", px cardWidth ) ]
            ]
            (List.map
                (\value ->
                    viewFieldInCard container fieldLabelWidth lookup value
                )
                values
            )
        ]


viewFieldInCard : FieldContainer -> Int -> Lookup -> Field.Model -> Html Msg
viewFieldInCard container labelWidth lookup value =
    let
        field =
            value.field
    in
    div [ class "card-field" ]
        [ div
            [ class "card-field-name"
            , style [ ( "width", px labelWidth ) ]
            ]
            [ label [ class "card-field-label" ]
                [ text (field.name ++ ": ") ]
            ]
        , div [ class "card-field-value" ]
            [ Field.view value
                |> Html.map (FieldMsg container value)
            ]
        ]


viewDetailTabs : Model -> Html Msg
viewDetailTabs model =
    let
        window =
            model.window

        mainTabName =
            window.mainTab.name

        selectedRow =
            model.selectedRow

        hasManyTabs =
            model.hasManyTabs

        indirectTabs =
            model.indirectTabs

        arenaArg =
            model.arenaArg

        hasManyDetailTabs =
            List.map
                (\tab ->
                    ( HasMany, tab, Nothing )
                )
                hasManyTabs

        indirectDetailTabs =
            List.map
                (\( linker, tab ) ->
                    ( Indirect, tab, Just linker )
                )
                indirectTabs

        detailTabs : List ( Section, Tab.Model, Maybe TableName )
        detailTabs =
            hasManyDetailTabs ++ indirectDetailTabs

        activeTab : Maybe ( Section, TableName, Maybe TableName )
        activeTab =
            case arenaArg.sectionTable of
                Just ( section, tableName ) ->
                    Just ( section, tableName, arenaArg.sectionViaLinker )

                Nothing ->
                    List.head detailTabs
                        |> Maybe.map
                            (\( section, tab, linker ) ->
                                ( section, tab.tab.tableName, linker )
                            )

        detailTabViews =
            List.map
                (\hasMany ->
                    let
                        isActive =
                            case activeTab of
                                Just ( activeSection, activeTable, activeLinker ) ->
                                    activeSection
                                        == HasMany
                                        && activeTable
                                        == hasMany.tab.tableName

                                Nothing ->
                                    False
                    in
                    listView isActive model.lookup HasMany hasMany
                )
                hasManyTabs
                ++ List.map
                    (\( linker, indirectTab ) ->
                        let
                            isActive =
                                case activeTab of
                                    Just ( activeSection, activeTable, activeLinker ) ->
                                        activeSection
                                            == Indirect
                                            && activeTable
                                            == indirectTab.tab.tableName
                                            && Just linker
                                            == activeLinker

                                    Nothing ->
                                        False
                        in
                        listView isActive model.lookup Indirect indirectTab
                    )
                    indirectTabs

        ( allotedWidth, allotedHeight ) =
            detailAllotedSize model

        adjustedWidth = allotedWidth + 50
    in
    if List.length detailTabs > 0 then
        div []
            [ div
                [ class "detail-tab-names-container"
                , style [ ( "width", px adjustedWidth  ) ]
                ]
                [ div [ class "detail-tab-names" ]
                    (List.map
                        (\( section, tabModel, linker ) ->
                            let
                                tab : Tab
                                tab =
                                    tabModel.tab

                                isActiveTab =
                                    case activeTab of
                                        Just ( activeSection, activeTable, activeLinker ) ->
                                            section
                                                == activeSection
                                                && activeTable
                                                == tab.tableName
                                                && linker
                                                == activeLinker

                                        Nothing ->
                                            False

                                arenaArg =
                                    model.arenaArg

                                -- Clicking will open the tab,
                                -- opening the tab in a new tab will open it in it's own window
                                tabLinkArenaArg =
                                    WindowArena.initArg (Just tab.tableName)

                                viaLinker =
                                    case linker of
                                        Just linker ->
                                            " , and are connected through " ++ linker.name

                                        Nothing ->
                                            ""

                                tooltipText =
                                    mainTabName ++ " has many " ++ tab.name ++ ", while " ++ tab.name ++ " can also have many " ++ mainTabName ++ viaLinker
                            in
                            a
                                [ class "detail-tab-name"
                                , classList
                                    [ ( "has-many-tab", section == HasMany )
                                    , ( "indirect-tab", section == Indirect )
                                    , ( "active-detail-tab", isActiveTab )
                                    ]
                                , Route.href (Route.WindowArena tabLinkArenaArg)
                                , onClickPreventDefault (ChangeActiveTab section tab.tableName linker)
                                ]
                                [ div [ class "tab-name-wrapper" ]
                                    [ text tab.name
                                    , div
                                        [ class "tab-relation tooltip"
                                        , classList
                                            [ ( "ion-network", section == Indirect )
                                            ]
                                        ]
                                        [ span [ class "tooltip-text" ]
                                            [ text tooltipText ]
                                        ]
                                    ]
                                ]
                        )
                        detailTabs
                    )
                ]
            , div [ class "detail-tabs" ]
                detailTabViews
            ]
    else
        text "No detail tabs"


listView : Bool -> Lookup -> Section -> Tab.Model -> Html Msg
listView isTabActive lookup section tab =
    let
        styleDisplay =
            case isTabActive of
                True ->
                    style [ ( "display", "block" ) ]

                False ->
                    style [ ( "display", "none" ) ]

        detailRecordView =
            Tab.view tab
                |> Html.map (\tabMsg -> TabMsg ( section, tab, tabMsg ))
    in
    div
        [ class "detail-tab"
        , styleDisplay
        ]
        [ detailRecordView ]


getPosition : Model -> Position
getPosition model =
    let
        position =
            model.position
    in
    case model.drag of
        Nothing ->
            position

        Just { start, current } ->
            Position
                (position.x + current.x - start.x)
                (position.y + current.y - start.y)


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown"
        (Decode.map
            (\p ->
                Drag (Start p)
            )
            Mouse.position
        )


type Drag
    = Start Position
    | At Position
    | End Position



-- UPDATE


type Msg
    = Drag Drag
    | ViewScrolled Scroll
    | TabMsg ( Section, Tab.Model, Tab.Msg )
    | TabMsgAll Tab.Msg
    | FieldMsg FieldContainer Field.Model Field.Msg
    | LookupNextPageReceived ( TableName, List Record )
    | LookupNextPageErrored String
    | RefreshTabPageReceived ( Section, Tab.Model, Rows )
    | RefreshTabPageError String
    | ChangeActiveTab Section TableName (Maybe TableName)
    | ToolbarMsg Toolbar.Msg
    | Maximize Bool
    | ClickedCloseButton
    | RecordChangesetUpdated RecordDetail
    | RecordChangesetUpdateError String
    | ContainerSizeChanged Size


updateDrag : Drag -> Model -> ( Model, Cmd Msg )
updateDrag drag model =
    case drag of
        Start xy ->
            let
                newModel =
                    { model | drag = Just (DragPosition xy xy) }
            in
            updateSizes newModel

        At xy ->
            let
                newModel =
                    { model
                        | drag = Maybe.map (\{ start } -> DragPosition start xy) model.drag
                    }
            in
            updateSizes newModel

        End _ ->
            let
                updatedModel0 =
                    { model
                        | position = getPosition model
                        , drag = Nothing
                    }

                split =
                    Util.roundDecimal 4 (splitPercentage updatedModel0)

                updatedArenaArg =
                    WindowArena.updateSplit split updatedModel0.arenaArg

                updatedModel1 =
                    { updatedModel0 | arenaArg = updatedArenaArg }

                ( updatedModel2, subCmd ) =
                    updateSizes updatedModel1
            in
            updatedModel2
                => Cmd.batch
                    [ subCmd

                    --, Route.modifyUrl (Route.WindowArena updatedModel2.arenaArg)
                    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        position =
            model.position

        drag =
            model.drag
    in
    case msg of
        Drag drag ->
            updateDrag drag model

        TabMsgAll tabMsg ->
            updateAllTabs tabMsg model

        TabMsg ( section, tabModel, Tab.ToolbarMsg Toolbar.ClickedNewButton ) ->
            let
                _ =
                    Debug.log "DetailedRecord: Clicked on NewRecordButton in tab:" tabModel.tab.name
            in
            model => Cmd.none

        TabMsg ( section, tabModel, Tab.SearchboxMsg searchbox searchboxMsg ) ->
            updateAndRefreshTab section tabModel (Tab.SearchboxMsg searchbox searchboxMsg) model

        TabMsg ( section, tabModel, Tab.ToggleSort columnName ) ->
            updateAndRefreshTab section tabModel (Tab.ToggleSort columnName) model

        TabMsg ( section, tabModel, tabMsg ) ->
            let
                ( updatedTabMode, updatedModel, cmd ) =
                    updateTab section tabModel tabMsg model
            in
            updatedModel => cmd

        FieldMsg Detail argField fieldMsg ->
            let
                valueUpdate =
                    updateFields fieldMsg argField model.values

                ( updatedFields, subCmd ) =
                    List.unzip valueUpdate
            in
            { model | values = updatedFields }
                => Cmd.batch subCmd

        FieldMsg OneOne argField fieldMsg ->
            let
                _ =
                    Debug.log "Field changed in OneOne in Tab: " argField.tab.name

                oneOneValueUpdate : List ( ( Tab, List Field.Model ), List (Cmd Msg) )
                oneOneValueUpdate =
                    List.map
                        (\( oneOneTab, oneOneValues ) ->
                            if oneOneTab == argField.tab then
                                let
                                    valueUpdate =
                                        updateFields fieldMsg argField oneOneValues

                                    ( updatedValues, subCmd ) =
                                        List.unzip valueUpdate
                                in
                                ( ( oneOneTab, updatedValues ), subCmd )
                            else
                                ( ( oneOneTab, [] ), [] )
                        )
                        model.oneOneValues

                ( updatedOneOneValues, subCmds ) =
                    List.unzip oneOneValueUpdate

                subCmd =
                    List.concat subCmds
            in
            { model | oneOneValues = updatedOneOneValues }
                => Cmd.batch subCmd

        LookupNextPageReceived ( sourceTable, recordList ) ->
            let
                updatedLookup =
                    Lookup.addPage sourceTable recordList model.lookup

                updatedModel =
                    { model
                        | lookup = updatedLookup
                        , dropdownPageRequestInFlight = False
                    }
            in
            updateAllFields (Field.LookupChanged updatedLookup) updatedModel

        LookupNextPageErrored e ->
            Debug.crash "Error loading next page lookup" e

        ChangeActiveTab section tableName linker ->
            let
                arenaArg =
                    model.arenaArg

                newArenaArg =
                    { arenaArg
                        | sectionTable = Just ( section, tableName )
                        , sectionViaLinker = linker
                    }
            in
            { model | arenaArg = newArenaArg }
                => Cmd.batch
                    [--Route.modifyUrl (Route.WindowArena newArenaArg)
                    ]

        ToolbarMsg Toolbar.ClickedSaveOnDetail ->
            let
                changeset =
                    getChangeset model

                _ =
                    Debug.log "Changeset:" changeset

                json =
                    Encode.encode 4 (DataContainer.changesetEncoder changeset)

                _ =
                    Debug.log "json" json

                settings =
                    model.settings

                tab =
                    model.window.mainTab

                tableName =
                    tab.tableName
            in
            model => requestUpdateRecords settings tableName changeset

        ToolbarMsg Toolbar.ClickedCancelOnDetail ->
            let
                _ =
                    Debug.log "Cancel changes on this record" ""

                ( updatedValues, subCmd ) =
                    cancelChangesOnValues model

                ( updatedOneOneValues, oneOneCmd ) =
                    cancelChangesOnOneOneValues model
            in
            { model
                | values = updatedValues
                , oneOneValues = updatedOneOneValues
            }
                => Cmd.batch (subCmd ++ oneOneCmd)

        -- handle this in WindowArena
        ToolbarMsg toolbarMsg ->
            model => Cmd.none

        Maximize v ->
            let
                newModel =
                    { model | isMaximized = v }

                ( updatedModel, cmd ) =
                    updateSizes newModel
            in
            updatedModel => cmd

        -- handled in WindowArena
        ClickedCloseButton ->
            model => Cmd.none

        RecordChangesetUpdated recordDetail ->
            let
                _ =
                    Debug.log "updated record" recordDetail
            in
            { model | selectedRow = Just recordDetail }
                => Cmd.none

        RecordChangesetUpdateError e ->
            { model | errors = model.errors ++ [ e ] }
                => Cmd.none

        ViewScrolled scroll ->
            let
                updatedModel =
                    { model | scroll = scroll }
            in
            updateAllFields (Field.ContainerScrollChanged scroll) updatedModel

        RefreshTabPageReceived ( section, tabModel, rows ) ->
            let
                ( updatedTabModel, updatedModel, cmd ) =
                    updateTab section tabModel (Tab.RefreshPageReceived rows) model
            in
            updatedModel => cmd

        RefreshTabPageError e ->
            { model | errors = model.errors ++ [ e ] }
                => Cmd.none

        ContainerSizeChanged size ->
            let
                updatedModel =
                    { model | containerSize = size }

                ( updatedModel2, subCmd2 ) =
                    updateSizes updatedModel

                ( allotedWidth, allotedHeight ) =
                    detailAllotedSize updatedModel2

                ( updatedModel3, subCmd3 ) =
                    updateAllFields (Field.AllotedTabWidthChanged (round allotedWidth)) updatedModel2
            in
            updatedModel3 => Cmd.batch [ subCmd2, subCmd3 ]


requestUpdateRecords : Settings -> TableName -> RecordDetailChangeset -> Cmd Msg
requestUpdateRecords settings tableName changeset =
    DataUpdate.updateRecord settings Nothing tableName changeset
        |> Http.toTask
        |> Task.attempt
            (\result ->
                case result of
                    Ok rows ->
                        RecordChangesetUpdated rows

                    Err e ->
                        RecordChangesetUpdateError (toString e)
            )


updateAllFields : Field.Msg -> Model -> ( Model, Cmd Msg )
updateAllFields fieldMsg model =
    let
        ( updatedValues, fieldSubCmd ) =
            List.map (Field.update fieldMsg) model.values
                |> List.unzip

        ( updatedOneOneValues, oneOnefieldSubCmd ) =
            List.map
                (\( tab, fields ) ->
                    let
                        ( updatedFields, subCmd ) =
                            List.map (Field.update fieldMsg) fields
                                |> List.unzip
                    in
                    ( ( tab, updatedFields ), subCmd )
                )
                model.oneOneValues
                |> List.unzip

        updatedModel =
            { model
                | values = updatedValues
                , oneOneValues = updatedOneOneValues
            }
    in
    updateAllTabs (Tab.AllRowMsg (Row.AllFieldMsg fieldMsg)) updatedModel


updateFields : Field.Msg -> Field.Model -> List Field.Model -> List ( Field.Model, Cmd Msg )
updateFields fieldMsg argValue fields =
    List.map
        (\value ->
            if argValue == value then
                let
                    ( newField, cmd ) =
                        Field.update fieldMsg value
                in
                ( newField, Cmd.map (FieldMsg OneOne newField) cmd )
            else
                value => Cmd.none
        )
        fields


resetFields : FieldContainer -> List Field.Model -> ( List Field.Model, List (Cmd Msg) )
resetFields container fields =
    List.map
        (\field ->
            let
                ( updatedField, subCmd ) =
                    Field.update Field.ResetChanges field
            in
            ( updatedField, Cmd.map (FieldMsg container updatedField) subCmd )
        )
        fields
        |> List.unzip


cancelChangesOnValues : Model -> ( List Field.Model, List (Cmd Msg) )
cancelChangesOnValues model =
    resetFields Detail model.values


cancelChangesOnOneOneValues : Model -> ( List ( Tab, List Field.Model ), List (Cmd Msg) )
cancelChangesOnOneOneValues model =
    let
        updatedFields =
            List.map
                (\( tab, values ) ->
                    let
                        ( updatedFields, subCmd ) =
                            resetFields OneOne values
                    in
                    ( ( tab, updatedFields ), subCmd )
                )
                model.oneOneValues

        ( oneOneValues, subCmds ) =
            List.unzip updatedFields
    in
    ( oneOneValues, List.concat subCmds )


requestNextPage : Section -> Tab.Model -> Model -> Cmd Msg
requestNextPage section tab model =
    let
        mainTable =
            model.window.mainTab.tableName

        arenaArg =
            model.arenaArg

        recordId =
            case arenaArg.action of
                WindowArena.Select recordId ->
                    Just recordId

                _ ->
                    Debug.crash "Can not request next page on detail other than selected record"

        tabPage =
            tab.currentPage

        sectionTable =
            tab.tab.tableName

        query =
            arenaArg.query

        pageQuery =
            Query.updatePage (tabPage + 1) query

        httpRequest =
            case recordId of
                Just recordId ->
                    case section of
                        HasMany ->
                            Records.fetchHasManyRecords model.settings mainTable recordId sectionTable pageQuery
                                |> Http.toTask

                        Indirect ->
                            Records.fetchIndirectRecords model.settings mainTable recordId sectionTable pageQuery
                                |> Http.toTask

                Nothing ->
                    Task.succeed Record.emptyRow
    in
    httpRequest
        |> Task.attempt
            (\result ->
                case result of
                    Ok rows ->
                        TabMsg ( section, tab, Tab.NextPageReceived rows )

                    Err e ->
                        TabMsg ( section, tab, Tab.NextPageError (toString e) )
            )


refreshTabPage : Section -> Tab.Model -> Model -> ( Model, Cmd Msg )
refreshTabPage section tabModel model =
    let
        arenaArg =
            model.arenaArg

        query =
            arenaArg.query

        pageQuery =
            Query.updatePage 1 query

        updatedArenaArg =
            { arenaArg | query = pageQuery }

        selectedRow =
            case model.selectedRow of
                Just recordDetail ->
                    Tab.getRecordIdString recordDetail.record model.window.mainTab

                Nothing ->
                    Debug.crash "There should be a selectedRecord"

        mainTable =
            case model.arenaArg.tableName of
                Just mainTable ->
                    mainTable

                Nothing ->
                    Debug.crash "There has to be a main table here"

        sectionTable =
            tabModel.tab.tableName

        request =
            case section of
                HasMany ->
                    Records.fetchHasManyRecords model.settings mainTable selectedRow sectionTable tabModel.query

                Indirect ->
                    Records.fetchIndirectRecords model.settings mainTable selectedRow sectionTable tabModel.query

        fetchCmd =
            request
                |> Http.toTask
                |> Task.attempt
                    (\result ->
                        case result of
                            Ok rows ->
                                RefreshTabPageReceived ( section, tabModel, rows )

                            Err e ->
                                RefreshTabPageError (toString e)
                    )
    in
    { model | arenaArg = updatedArenaArg }
        => fetchCmd


updateSizes : Model -> ( Model, Cmd Msg )
updateSizes model =
    let
        realPosition =
            getPosition model

        window =
            model.window

        ( mainRecordHeight, detailTabHeight ) =
            splitTabHeights window realPosition model.isMaximized model.containerSize

        ( allotedWidth, allotedHeight ) =
            detailAllotedSize model

        tabSize =
            ( allotedWidth, detailTabHeight )
    in
    update (TabMsgAll (Tab.SetSize tabSize)) model


updateValues : Field.Msg -> Model -> ( Model, Cmd Msg )
updateValues fieldMsg model =
    let
        ( updatedValues, fieldSubCmd ) =
            List.map (Field.update fieldMsg) model.values
                |> List.unzip
    in
    { model | values = updatedValues }
        => Cmd.batch
            (List.map2
                (\value subCmd ->
                    Cmd.map (FieldMsg Detail value) subCmd
                )
                updatedValues
                fieldSubCmd
            )


updateAndRefreshTab : Section -> Tab.Model -> Tab.Msg -> Model -> ( Model, Cmd Msg )
updateAndRefreshTab section tabModel tabMsg model =
    let
        ( updatedTabModel, updatedModel, subCmd ) =
            updateTab section tabModel tabMsg model

        ( updatedModel2, subCmd2 ) =
            refreshTabPage section updatedTabModel updatedModel
    in
    updatedModel2 => Cmd.batch [ subCmd, subCmd2 ]


updateTab : Section -> Tab.Model -> Tab.Msg -> Model -> ( Tab.Model, Model, Cmd Msg )
updateTab section tabModel tabMsg model =
    let
        ( newTabModel, subCmd ) =
            Tab.update tabMsg tabModel

        action =
            model.arenaArg.action

        doRequestPage =
            case action of
                Select _ ->
                    requestNextPage section newTabModel model

                _ ->
                    Cmd.none

        ( updatedTabModel, tabCmd ) =
            case Tab.pageRequestNeeded newTabModel of
                True ->
                    { newTabModel | pageRequestInFlight = True }
                        => doRequestPage

                False ->
                    newTabModel => Cmd.none

        ( updatedHasManyTabs, hasManyCmds ) =
            updateTabModels tabMsg model.hasManyTabs updatedTabModel
                |> List.unzip

        ( updatedIndirectTabs, indirectCmds ) =
            updateIndirectTabModels tabMsg model.indirectTabs updatedTabModel
                |> List.unzip

        updatedTabCmd =
            Cmd.map (\tabMsg -> TabMsg ( section, updatedTabModel, tabMsg )) subCmd

        detailTabCmds =
            (List.map2
                (\hasManyModel hasManyCmd ->
                    Cmd.map
                        (\tabMsg ->
                            TabMsg ( HasMany, hasManyModel, tabMsg )
                        )
                        hasManyCmd
                )
                updatedHasManyTabs
                hasManyCmds
                ++ List.map2
                    (\( linker, indirectModel ) hasManyCmd ->
                        Cmd.map
                            (\tabMsg ->
                                TabMsg ( Indirect, indirectModel, tabMsg )
                            )
                            hasManyCmd
                    )
                    updatedIndirectTabs
                    indirectCmds
            )
                |> Cmd.batch
    in
    ( updatedTabModel
    , { model
        | hasManyTabs = updatedHasManyTabs
        , indirectTabs = updatedIndirectTabs
      }
    , Cmd.batch
        [ tabCmd
        , updatedTabCmd
        , detailTabCmds
        ]
    )


updateAllTabs : Tab.Msg -> Model -> ( Model, Cmd Msg )
updateAllTabs tabMsg model =
    let
        ( updatedModel, cmd ) =
            updateHasManyTabs tabMsg model

        ( updatedModel2, cmd2 ) =
            updateIndirectTabs tabMsg updatedModel
    in
    updatedModel2 => Cmd.batch [ cmd, cmd2 ]


updateHasManyTabs : Tab.Msg -> Model -> ( Model, Cmd Msg )
updateHasManyTabs tabMsg model =
    let
        ( updatedTabs, tabSubCmd ) =
            List.map (Tab.update tabMsg) model.hasManyTabs
                |> List.unzip
    in
    { model | hasManyTabs = updatedTabs }
        => Cmd.batch
            (List.map2
                (\tab subCmd ->
                    Cmd.map
                        (\subMsg ->
                            TabMsg ( HasMany, tab, subMsg )
                        )
                        subCmd
                )
                updatedTabs
                tabSubCmd
            )


updateIndirectTabs : Tab.Msg -> Model -> ( Model, Cmd Msg )
updateIndirectTabs tabMsg model =
    let
        ( updatedTabs, tabSubCmd ) =
            List.map
                (\( linker, tab ) ->
                    let
                        ( updatedTab, subCmd ) =
                            Tab.update tabMsg tab
                    in
                    ( ( linker, updatedTab ), subCmd )
                )
                model.indirectTabs
                |> List.unzip
    in
    { model | indirectTabs = updatedTabs }
        => Cmd.batch
            (List.map2
                (\( linker, tab ) subCmd ->
                    Cmd.map
                        (\subMsg ->
                            TabMsg ( Indirect, tab, subMsg )
                        )
                        subCmd
                )
                updatedTabs
                tabSubCmd
            )


updateTabModels : Tab.Msg -> List Tab.Model -> Tab.Model -> List ( Tab.Model, Cmd Tab.Msg )
updateTabModels tabMsg modelList tabModel =
    List.map
        (\model ->
            if model.tab.tableName == tabModel.tab.tableName then
                Tab.update tabMsg model
            else
                model => Cmd.none
        )
        modelList


updateIndirectTabModels : Tab.Msg -> List ( TableName, Tab.Model ) -> Tab.Model -> List ( ( TableName, Tab.Model ), Cmd Tab.Msg )
updateIndirectTabModels tabMsg modelList tabModel =
    List.map
        (\( linker, model ) ->
            if model.tab.tableName == tabModel.tab.tableName then
                let
                    ( updatedTab, cmd ) =
                        Tab.update tabMsg model
                in
                ( ( linker, updatedTab ), cmd )
            else
                ( ( linker, model ), Cmd.none )
        )
        modelList



-- SUBSCRIPTION --


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dividerHeightSubscriptions model
        ]


dividerHeightSubscriptions : Model -> Sub Msg
dividerHeightSubscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Sub.map Drag (Mouse.moves At), Sub.map Drag (Mouse.ups End) ]
