module Views.Window.Tab
    exposing
        ( Model
        , Msg(..)
        , dropdownPageRequestNeeded
        , editedRows
        , getLinkExistingRows
        , getLinkNewRows
        , getUnlinkedRows
        , init
        , isModified
        , listView
        , pageRequestNeeded
        , selectedRowCount
        , selectedRows
        , subscriptions
        , update
        )

import Constant
import Data.DataContainer as DataContainer exposing (SaveContainer)
import Data.Query as Query exposing (Query)
import Data.Query.Filter as Filter
import Data.Query.Order as Order exposing (Order)
import Data.Query.Sort as Sort exposing (Sort)
import Data.Window.ColumnName as ColumnName
import Data.Window.Field as Field exposing (Field)
import Data.Window.Lookup as Lookup exposing (Lookup)
import Data.Window.Presentation as Presentation exposing (Presentation(..))
import Data.Window.Record as Record exposing (Record, RecordId, Rows)
import Data.Window.Tab as Tab exposing (Tab, TabType)
import Data.Window.TableName as TableName exposing (TableName)
import Data.WindowArena as WindowArena exposing (Action(..), ArenaArg)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (attribute, checked, class, classList, href, id, placeholder, property, src, style, type_)
import Html.Events exposing (onCheck, onClick)
import Http
import Ionicon
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Request.Window.DataUpdate as DataUpdate
import Request.Window.Records
import Settings exposing (Settings)
import Task exposing (Task)
import Util exposing ((=>), Scroll, onScroll, px, viewIf)
import Views.Window.Field as Field
import Views.Window.LinkRow as LinkRow
import Views.Window.Row as Row
import Views.Window.Searchbox as Searchbox
import Views.Window.Toolbar as Toolbar


type alias Model =
    { tab : Tab
    , tabType : TabType
    , scroll : Scroll
    , size : ( Float, Float )
    , pageRows : List (List Row.Model)
    , linkRows : List LinkRow.Model
    , newRows : List Row.Model
    , pageRequestInFlight : Bool
    , currentPage : Int
    , reachedLastPage : Bool
    , totalRecords : Int
    , isMultiSort : Bool
    , query : Query
    , selectedRecordId : Maybe RecordId
    , unlinked : List Row.Model
    , settings : Settings
    , arenaArg : ArenaArg
    , errors : List String
    }


isModified : Model -> Bool
isModified model =
    let
        hasUnlinked =
            List.length model.unlinked > 0

        hasNewRows =
            List.length model.newRows > 0

        hasLinkRows =
            List.length model.linkRows > 0
    in
    hasUnlinked || hasNewRows || hasLinkRows || hasModifiedRows model


{-|

    return the modification of the record in this tab

-}
getForSave : Model -> SaveContainer
getForSave model =
    let
        tab =
            model.tab

        tableName =
            tab.tableName
    in
    { forInsert = ( tableName, getNewRows model )
    , forUpdate = ( tableName, editedRows model )
    }


hasModifiedRows : Model -> Bool
hasModifiedRows model =
    List.any
        (\page ->
            List.any
                (\row ->
                    Row.isModified row
                )
                page
        )
        model.pageRows


{-|

    return the modified rows

-}
editedRows : Model -> Rows
editedRows model =
    let
        columns =
            Tab.columnNames model.tab

        listRecords =
            List.map
                (\page ->
                    List.filterMap
                        (\row ->
                            if Row.isModified row then
                                Just (Row.editedRecord row)
                            else
                                Nothing
                        )
                        page
                )
                model.pageRows
                |> List.concat
    in
    Record.listRecordToRows columns listRecords


{-|

    return the newly inserted rows

-}
getNewRows : Model -> Rows
getNewRows model =
    let
        columns =
            Tab.columnNames model.tab

        newRows =
            List.map
                (\newRow ->
                    Row.editedRecord newRow
                )
                model.newRows
    in
    Record.listRecordToRows columns newRows


{-|

    return the new inserted rows

-}
getLinkNewRows : Model -> Rows
getLinkNewRows model =
    let
        columns =
            Tab.columnNames model.tab

        listRecords =
            List.map
                (\newRow ->
                    Row.editedRecord newRow
                )
                model.newRows
    in
    Record.listRecordToRows columns listRecords


{-|

    return the unlinked new rows

-}
getUnlinkedRows : Model -> Rows
getUnlinkedRows model =
    let
        columns =
            Tab.columnNames model.tab

        listRecords =
            List.map
                (\newRow ->
                    Row.editedRecord newRow
                )
                model.unlinked
    in
    Record.listRecordToRows columns listRecords


{-|

    return the rows linked existing records

-}
getLinkExistingRows : Model -> Rows
getLinkExistingRows model =
    let
        values =
            List.filterMap
                (\linkRow ->
                    LinkRow.getLinkRow linkRow
                )
                model.linkRows

        tab =
            model.tab

        display =
            case tab.display of
                Just display ->
                    display

                Nothing ->
                    Debug.crash "This tab doesn't have a identifier display"

        displayPk =
            List.map
                (\pk ->
                    ColumnName.completeName pk
                )
                display.pk

        rows =
            { columns = displayPk
            , data = [ values ]
            , count = Nothing
            }
    in
    rows


init : ArenaArg -> Settings -> Maybe RecordId -> ( Float, Float ) -> Query -> Tab -> TabType -> Rows -> Model
init arenaArg settings selectedRecordId size query tab tabType rows =
    { tab = tab
    , tabType = tabType
    , scroll = Scroll 0 0
    , size = size
    , pageRows = [ createRowsModel selectedRecordId tab rows ]
    , linkRows = []
    , newRows = []
    , pageRequestInFlight = False
    , currentPage = 1
    , reachedLastPage = False
    , totalRecords =
        case rows.count of
            Just count ->
                count

            Nothing ->
                0
    , query = query
    , isMultiSort =
        case query.sort of
            Just sort ->
                Sort.isMultiSort sort

            Nothing ->
                False
    , selectedRecordId = selectedRecordId
    , unlinked = []
    , settings = settings
    , arenaArg = arenaArg
    , errors = []
    }


createRowsModel : Maybe RecordId -> Tab -> Rows -> List Row.Model
createRowsModel selectedRecordId tab rows =
    let
        recordList =
            Record.rowsToRecordList rows
    in
    List.map
        (\record ->
            let
                recordId =
                    Tab.recordId record tab
            in
            Row.init WindowArena.ListPage recordId record tab
        )
        recordList


numberOfRecords : Model -> Int
numberOfRecords model =
    List.foldl
        (\page len ->
            len + List.length page
        )
        0
        model.pageRows


{-| IMPORTANT: rowHeight 40 is based on the
computed tab-row css class, not matching the rowHeight will make the load-page-on-deman upon
scoll not trigger since isScrolledBottom does not measure the actual value
-}
estimatedListHeight : Model -> Float
estimatedListHeight model =
    let
        rowHeight =
            Constant.tabRowValueHeight

        rowLength =
            numberOfRecords model
    in
    rowHeight * toFloat rowLength


{-| The list is scrolled to Bottom, this is an estimated calculation
based on content list height and the scroll of content
when scrollTop + tabHeight > totalListHeight - bottomAllowance
-}
isScrolledBottom : Model -> Bool
isScrolledBottom model =
    let
        contentHeight =
            estimatedListHeight model

        scrollTop =
            model.scroll.top

        bottomAllowance =
            50.0

        ( width, height ) =
            model.size
    in
    --Debug.log ("scrollTop("++toString scrollTop++") + model.height("++toString model.height ++") > contentHeight("++toString contentHeight++") - bottomAllowance("++toString bottomAllowance++")")
    scrollTop + height > contentHeight - bottomAllowance


pageRequestNeeded : Model -> Bool
pageRequestNeeded model =
    let
        needed =
            isScrolledBottom model && not model.pageRequestInFlight && not model.reachedLastPage

        {-
           _ =
               Debug.log
                   ("in pageRequestNeeded --> isScrolledBottom: "
                       ++ toString (isScrolledBottom model)
                       ++ " pageReqeustInFlight: "
                       ++ toString model.pageRequestInFlight
                       ++ " reachedLastPage: "
                       ++ toString model.reachedLastPage
                   )
                   needed
        -}
    in
    needed


dropdownPageRequestNeeded : Lookup -> Model -> Maybe TableName
dropdownPageRequestNeeded lookup model =
    let
        rowDropdown =
            List.filterMap
                (\page ->
                    List.filterMap
                        (\row ->
                            Row.dropdownPageRequestNeeded lookup row
                        )
                        page
                        |> List.head
                )
                model.pageRows

        linkRowDropdown =
            List.filterMap
                (\linkRow ->
                    LinkRow.dropdownPageRequestNeeded lookup linkRow
                )
                model.linkRows

        newRowDropdown =
            List.filterMap
                (\newRow ->
                    Row.dropdownPageRequestNeeded lookup newRow
                )
                model.newRows
    in
    rowDropdown
        ++ newRowDropdown
        ++ linkRowDropdown
        |> List.head


listView : Lookup -> Model -> Html Msg
listView lookup model =
    let
        tab =
            model.tab

        fields =
            tab.fields

        ( width, height ) =
            model.size

        adjustedWidth =
            adjustWidth width model

        tabType =
            model.tabType

        toolbarModel =
            { selected = selectedRowCount model
            , modified = countAllModifiedRows model
            , showIconText = width > Constant.showIconTextMinWidth
            , moveDownIconText = width > Constant.showIconTextMinWidth && width < Constant.moveDownIconTextMinWidth
            , multiColumnSort = model.isMultiSort
            }

        viewToolbar =
            case tabType of
                Tab.InMain ->
                    Toolbar.viewForMain toolbarModel
                        |> Html.map ToolbarMsg

                Tab.InHasMany ->
                    Toolbar.viewForHasMany toolbarModel
                        |> Html.map ToolbarMsg

                Tab.InIndirect ->
                    Toolbar.viewForIndirect toolbarModel
                        |> Html.map ToolbarMsg
    in
    div []
        [ div
            [ class "toolbar-area"
            ]
            [ viewToolbar ]
        , div
            [ class "tab-list-view"
            ]
            [ div [ class "frozen-head-columns" ]
                [ viewFrozenHead model
                , viewColumns model fields
                ]
            , div [ class "page-shadow-and-list-rows" ]
                [ viewPageShadow model
                , div
                    [ class "list-view-rows"
                    , onScroll ListRowScrolled
                    , style
                        [ ( "height", px height )
                        , ( "width", px adjustedWidth )
                        ]
                    ]
                    ([ viewLinkRows lookup model ]
                        ++ [ viewNewRows lookup model ]
                        ++ [ listViewRows lookup model ]
                    )
                ]
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
    if model.pageRequestInFlight then
        div
            [ class "loading-indicator spin animated fadeInUp"
            ]
            [ Ionicon.loadB iconSize iconColor ]
    else
        text ""


viewPageShadow : Model -> Html Msg
viewPageShadow model =
    let
        scrollTop =
            model.scroll.top

        topPx =
            px -scrollTop

        tab =
            model.tab

        ( width, height ) =
            model.size
    in
    div
        [ class "page-shadow"
        , style [ ( "height", px height ) ]
        ]
        [ div
            [ class "page-shadow-content"
            , style [ ( "top", topPx ) ]
            ]
            (List.map
                (\page ->
                    div [ class "shadow-page" ]
                        [ viewRowShadow page model.tab ]
                )
                model.pageRows
            )
        ]


allRows : Model -> List Row.Model
allRows model =
    List.concat model.pageRows


selectedRows : Model -> List Row.Model
selectedRows model =
    allRows model
        |> List.filter .selected


selectedRowCount : Model -> Int
selectedRowCount model =
    selectedRows model
        |> List.length


countAllModifiedRows : Model -> Int
countAllModifiedRows model =
    List.foldl
        (\page sum ->
            sum + countRowModifiedInPage page
        )
        0
        model.pageRows


countRowModifiedInPage : List Row.Model -> Int
countRowModifiedInPage pageRow =
    List.filter Row.isModified pageRow
        |> List.length


viewRowShadow : List Row.Model -> Tab -> Html Msg
viewRowShadow pageRow tab =
    div [ class "row-shadow" ]
        (List.map
            (\row ->
                Row.viewRowControls row row.recordId tab
                    |> Html.map (RowMsg row)
            )
            pageRow
        )


viewFrozenHead : Model -> Html Msg
viewFrozenHead model =
    let
        loadedItems =
            numberOfRecords model

        totalItems =
            model.totalRecords

        itemsIndicator =
            toString loadedItems ++ "/" ++ toString totalItems
    in
    div
        [ class "frozen-head"
        ]
        [ div [ class "frozen-head-indicator" ]
            [ div [] [ text itemsIndicator ]
            , div [ class "sort-order-reset" ]
                [ i [ class "fa fa-circle-thin" ] []
                ]
            ]
        , div
            [ class "frozen-head-controls" ]
            [ input
                [ type_ "checkbox"
                , onCheck ToggleSelectAllRows
                , checked False
                ]
                []
            , div [ class "filter-btn" ]
                [ i [ class "fa fa-filter" ] [] ]
            ]
        ]


adjustWidth : Float -> Model -> Float
adjustWidth width model =
    let
        rowShadowWidth =
            120

        totalDeductions =
            rowShadowWidth
    in
    width - totalDeductions


viewColumns : Model -> List Field -> Html Msg
viewColumns model fields =
    let
        scrollLeft =
            model.scroll.left

        leftPx =
            px -scrollLeft

        ( width, height ) =
            model.size

        adjustedWidth =
            adjustWidth width model
    in
    div
        [ class "tab-columns"
        , style [ ( "width", px adjustedWidth ) ]
        ]
        [ div
            [ class "tab-columns-content"
            , style [ ( "left", leftPx ) ]
            ]
            (List.map (viewColumnWithSearchbox model) fields)
        ]


viewColumnWithSearchbox : Model -> Field -> Html Msg
viewColumnWithSearchbox model field =
    let
        query =
            model.query

        filter =
            query.filter

        condition =
            filter

        columnName =
            Field.firstColumnName field

        searchValue =
            case condition of
                Just condition ->
                    Filter.get columnName condition

                Nothing ->
                    Nothing

        searchboxModel =
            Searchbox.init field searchValue

        sort =
            query.sort

        ( widthClass, widgetWidth, widgetHeight ) =
            Field.calcWidgetSize 0 Presentation.InList field

        columnWidth =
            widgetWidth + Constant.columnPad
    in
    div
        [ class "tab-column-with-filter"
        , style [ ( "width", px columnWidth ) ]
        ]
        [ viewColumn model.isMultiSort field sort
        , Searchbox.view searchboxModel
            |> Html.map (SearchboxMsg searchboxModel)
        ]


viewColumn : Bool -> Field -> Maybe Sort -> Html Msg
viewColumn isMultiSort field sort =
    let
        columnName =
            Field.columnName field

        findIndex list columnName =
            List.indexedMap
                (\index o ->
                    if o.column == columnName then
                        Just ( index, o.direction )
                    else
                        Nothing
                )
                list
                |> List.filter
                    (\a ->
                        case a of
                            Just _ ->
                                True

                            Nothing ->
                                False
                    )
                |> List.head

        sortOrder =
            case sort of
                Just list ->
                    case findIndex list columnName of
                        Just n ->
                            n

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
    in
    div
        [ class "tab-column-with-sort"
        , onClick (ToggleSort columnName)
        ]
        [ div [ class "tab-column" ]
            ([ div [ class "column-name" ]
                [ text columnName ]
             ]
                ++ viewSortOrder isMultiSort sortOrder
            )
        ]


viewSortOrder : Bool -> Maybe ( Int, Order.Direction ) -> List (Html Msg)
viewSortOrder isMultiSort sortOrder =
    case sortOrder of
        Just ( sortOrder, direction ) ->
            [ div [ class "column-sort" ]
                [ div [ class "sort-btn asc" ]
                    [ i [ class "fa fa-sort-asc" ] []
                    ]
                    |> viewIf (direction == Order.ASC)
                , div [ class "sort-btn desc" ]
                    [ i [ class "fa fa-sort-desc" ] []
                    ]
                    |> viewIf (direction == Order.DESC)
                ]
            , div [ class "sort-order-badge" ]
                [ text (toString (sortOrder + 1)) ]
                |> viewIf isMultiSort
            ]

        Nothing ->
            []


viewPage : Lookup -> List Row.Model -> Html Msg
viewPage lookup rowList =
    div []
        (List.map
            (\row ->
                Row.view lookup row
                    |> Html.map (RowMsg row)
            )
            rowList
        )


viewNewRows : Lookup -> Model -> Html Msg
viewNewRows lookup model =
    div []
        (List.map
            (\newRow ->
                Row.view lookup newRow
                    |> Html.map (NewRowMsg newRow)
            )
            model.newRows
        )


viewLinkRows : Lookup -> Model -> Html Msg
viewLinkRows lookup model =
    let
        tab =
            model.tab
    in
    div []
        (List.map
            (\linkRow ->
                LinkRow.view lookup linkRow
                    |> Html.map (LinkRowMsg linkRow)
            )
            model.linkRows
        )


listViewRows : Lookup -> Model -> Html Msg
listViewRows lookup model =
    let
        tab =
            model.tab
    in
    div [ class "tab-page" ]
        (if List.length model.pageRows > 0 then
            List.map
                (\pageRow ->
                    viewPage lookup pageRow
                )
                model.pageRows
         else
            [ div [ class "empty-list-view-rows" ]
                [ text "Empty list view rows" ]
            ]
        )


type Msg
    = SetSize ( Float, Float )
    | ListRowScrolled Scroll
    | NextPageReceived Rows
    | NextPageError String
    | RefreshPageReceived Rows
    | SaveChangesSucceed Rows
    | SaveChangesErrored String
    | RefreshPageError String
    | RowMsg Row.Model Row.Msg
    | NewRowMsg Row.Model Row.Msg
    | LinkRowMsg LinkRow.Model LinkRow.Msg
    | SearchboxMsg Searchbox.Model Searchbox.Msg
    | ToggleSelectAllRows Bool
    | ToolbarMsg Toolbar.Msg
    | ToggleSort String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSize size ->
            { model | size = size } => Cmd.none

        ListRowScrolled scroll ->
            { model | scroll = scroll } => Cmd.none

        NextPageReceived rows ->
            if List.length rows.data > 0 then
                { model
                    | pageRows = model.pageRows ++ [ createRowsModel model.selectedRecordId model.tab rows ]
                    , pageRequestInFlight = False
                    , currentPage = model.currentPage + 1
                }
                    => Cmd.none
            else
                { model
                    | reachedLastPage = True
                    , pageRequestInFlight = False
                }
                    => Cmd.none

        NextPageError e ->
            let
                _ =
                    Debug.log "Error receiving next page"
            in
            model => Cmd.none

        RefreshPageReceived rows ->
            { model
                | pageRows = [ createRowsModel model.selectedRecordId model.tab rows ]
                , pageRequestInFlight = False

                -- any change to search/filter will have to reset the current page
                , currentPage = 1
                , reachedLastPage = False
            }
                => Cmd.none

        RefreshPageError e ->
            { model | errors = model.errors ++ [ e ] }
                => Cmd.none

        SaveChangesSucceed rows ->
            let
                updatedModel =
                    { model | newRows = [] }
            in
            updatedModel
                => refreshPageRows updatedModel

        SaveChangesErrored e ->
            { model | errors = model.errors ++ [ e ] }
                => Cmd.none

        RowMsg argRow rowMsg ->
            let
                updatedPage : List (List ( Row.Model, Cmd Msg ))
                updatedPage =
                    List.map
                        (\page ->
                            List.map
                                (\row ->
                                    let
                                        ( newRow, subCmd ) =
                                            if row == argRow then
                                                Row.update rowMsg row
                                            else
                                                ( row, Cmd.none )
                                    in
                                    ( newRow, Cmd.map (RowMsg newRow) subCmd )
                                )
                                page
                        )
                        model.pageRows

                ( pageRows, subCmd ) =
                    List.foldl
                        (\listList ( pageAcc, cmdAcc ) ->
                            let
                                ( page, cmd ) =
                                    List.unzip listList
                            in
                            ( pageAcc ++ [ page ], cmdAcc ++ cmd )
                        )
                        ( [], [] )
                        updatedPage
            in
            { model | pageRows = pageRows } => Cmd.batch subCmd

        NewRowMsg argNewRow rowMsg ->
            let
                ( updatedNewRows, subCmd ) =
                    List.map
                        (\newRow ->
                            let
                                ( updatedRow, cmd ) =
                                    if newRow == argNewRow then
                                        Row.update rowMsg newRow
                                    else
                                        ( newRow, Cmd.none )
                            in
                            ( updatedRow, Cmd.map (NewRowMsg updatedRow) cmd )
                        )
                        model.newRows
                        |> List.unzip
            in
            { model | newRows = updatedNewRows } => Cmd.batch subCmd

        LinkRowMsg argLinkRow linkRowMsg ->
            let
                ( updatedLinkRows, subCmds ) =
                    List.map
                        (\linkRow ->
                            let
                                ( updatedRow, cmd ) =
                                    if linkRow == argLinkRow then
                                        let
                                            _ =
                                                Debug.log "updating this linkRow"
                                        in
                                        LinkRow.update linkRowMsg linkRow
                                    else
                                        ( linkRow, Cmd.none )
                            in
                            ( updatedRow, Cmd.map (LinkRowMsg updatedRow) cmd )
                        )
                        model.linkRows
                        |> List.unzip
            in
            { model | linkRows = updatedLinkRows }
                => Cmd.batch subCmds

        SearchboxMsg searchbox msg ->
            let
                ( newSearchbox, subCmd ) =
                    Searchbox.update searchbox msg

                field =
                    newSearchbox.field

                columnName =
                    Field.firstColumnName field

                searchValue =
                    Searchbox.getSearchText newSearchbox

                query =
                    model.query

                filter =
                    query.filter

                updatedQuery =
                    case searchValue of
                        -- remove the filter for a column when search value is empty
                        Just "" ->
                            Query.removeFromFilter columnName query

                        Just searchValue ->
                            Query.putToFilter columnName searchValue query

                        Nothing ->
                            query
            in
            { model
                | query = updatedQuery
                , currentPage = 0
            }
                => Cmd.none

        ToggleSelectAllRows v ->
            let
                ( pageRows, cmds ) =
                    toggleSelectAllRows v model.pageRows
            in
            { model | pageRows = pageRows }
                => Cmd.batch cmds

        ToolbarMsg Toolbar.ClickedSaveButton ->
            let
                forSave =
                    getForSave model

                json =
                    Encode.encode 4 (DataContainer.containerEncoder forSave)

                _ =
                    Debug.log "for save:" forSave

                _ =
                    Debug.log ("For Save:" ++ json) ""

                settings =
                    model.settings

                tab =
                    model.tab

                tableName =
                    tab.tableName
            in
            model
                => Cmd.batch
                    [ requestSaveTabRowsChanges settings tableName forSave
                    ]

        ToolbarMsg Toolbar.ClickedRefresh ->
            model => refreshPageRows model

        ToolbarMsg Toolbar.ToggleMultiSort ->
            { model | isMultiSort = not model.isMultiSort }
                => Cmd.none

        ToolbarMsg Toolbar.ClickedResetMultiSort ->
            { model | query = Query.removeSort model.query }
                => Cmd.none

        ToolbarMsg Toolbar.ClickedCancelOnMain ->
            let
                ( updatedPageRows, subCmd ) =
                    resetPageRows model
            in
            { model | pageRows = updatedPageRows }
                => Cmd.batch subCmd

        ToolbarMsg Toolbar.ClickedInsertNewButton ->
            insertNewRow model

        ToolbarMsg Toolbar.ClickedLinkNewRecord ->
            insertNewRow model

        ToolbarMsg Toolbar.ClickedLinkExisting ->
            let
                linkRowId =
                    List.length model.linkRows

                linkRow =
                    LinkRow.init linkRowId model.tab
            in
            { model | linkRows = model.linkRows ++ [ linkRow ] }
                => Cmd.none

        ToolbarMsg Toolbar.ClickedMainDelete ->
            let
                selected =
                    selectedRows model

                updatedPageRows =
                    List.map
                        (\page ->
                            List.filterMap
                                (\row ->
                                    if List.member row selected then
                                        Nothing
                                    else
                                        Just row
                                )
                                page
                        )
                        model.pageRows

                _ =
                    Debug.log "unlinked" selected
            in
            { model
                | unlinked = selected
                , pageRows = updatedPageRows
            }
                => Cmd.none

        ToolbarMsg toolbarMsg ->
            let
                _ =
                    Debug.log "toolbarMsg" toolbarMsg
            in
            model => Cmd.none

        {-
           SetFocusedRecord recordId ->
               let
                   newModel =
                       { model | selectedRecordId = Just recordId }

                   ( pageRows, cmds ) =
                       updateAllRowsSetFocusedRecord recordId newModel.pageRows
               in
               { newModel | pageRows = pageRows }
                   => Cmd.batch cmds
        -}
        ToggleSort columnName ->
            let
                _ =
                    Debug.log "toggleSort: " columnName

                updatedQuery =
                    if model.isMultiSort then
                        Query.updateSort columnName model.query
                    else
                        Query.setColumnSort columnName model.query

                _ =
                    Debug.log "tab updatedQuery: " updatedQuery
            in
            { model
                | query = updatedQuery
                , pageRequestInFlight = True -- since this will trigger refreshPage in Window.elm
            }
                => Cmd.none


requestSaveTabRowsChanges : Settings -> TableName -> SaveContainer -> Cmd Msg
requestSaveTabRowsChanges settings tableName forSave =
    DataUpdate.updateTab settings Nothing tableName forSave
        |> Http.toTask
        |> Task.attempt
            (\result ->
                let
                    _ =
                        Debug.log "save change result:" result
                in
                case result of
                    Ok rows ->
                        SaveChangesSucceed rows

                    Err e ->
                        SaveChangesErrored (toString e)
            )


refreshPageRows : Model -> Cmd Msg
refreshPageRows model =
    let
        _ =
            Debug.log "refreshing in tab" ""

        arenaArg =
            model.arenaArg

        query =
            arenaArg.query

        pageQuery =
            Query.updatePage 1 query

        tab =
            model.tab

        request =
            Request.Window.Records.listPageWithQuery model.settings Nothing tab.tableName pageQuery
    in
    request
        |> Http.toTask
        |> Task.attempt
            (\result ->
                case result of
                    Ok rows ->
                        RefreshPageReceived rows

                    Err e ->
                        RefreshPageError (toString e)
            )


{-| insert a new row model into the page
-}
insertNewRow : Model -> ( Model, Cmd Msg )
insertNewRow model =
    let
        newRowId =
            List.length model.newRows

        newRecordId =
            Record.TempLocal newRowId

        emptyRecord =
            Record.empty

        newRow =
            Row.init (WindowArena.NewRecord Presentation.InList) newRecordId emptyRecord model.tab
    in
    { model | newRows = model.newRows ++ [ newRow ] }
        => Cmd.none


resetPageRows : Model -> ( List (List Row.Model), List (Cmd Msg) )
resetPageRows model =
    let
        ( updatedPageRow, subCmd ) =
            List.map
                (\page ->
                    List.map
                        (\row ->
                            let
                                ( updatedRow, rowCmd ) =
                                    Row.update Row.ResetChanges row
                            in
                            ( updatedRow, Cmd.map (RowMsg updatedRow) rowCmd )
                        )
                        page
                        |> List.unzip
                )
                model.pageRows
                |> List.unzip
    in
    ( updatedPageRow, List.concat subCmd )


toggleSelectAllRows : Bool -> List (List Row.Model) -> ( List (List Row.Model), List (Cmd Msg) )
toggleSelectAllRows value pageList =
    let
        ( updatedRowModel, rowCmds ) =
            List.map
                (\page ->
                    List.map
                        (\row ->
                            let
                                ( updatedRow, rowCmd ) =
                                    Row.update (Row.ToggleSelect value) row
                            in
                            ( updatedRow, rowCmd |> Cmd.map (RowMsg updatedRow) )
                        )
                        page
                        |> List.unzip
                )
                pageList
                |> List.unzip
    in
    ( updatedRowModel, List.concat rowCmds )



{-
   updateAllRowsSetFocusedRecord : RecordId -> List (List Row.Model) -> ( List (List Row.Model), List (Cmd Msg) )
   updateAllRowsSetFocusedRecord recordId pageList =
       let
           ( updatedRowModel, rowCmds ) =
               List.map
                   (\page ->
                       List.map
                           (\row ->
                               let
                                   isFocused =
                                       row.recordId == recordId

                                   ( updatedRow, rowCmd ) =
                                       Row.update (Row.SetFocused isFocused) row
                               in
                               ( updatedRow, rowCmd |> Cmd.map (RowMsg updatedRow) )
                           )
                           page
                           |> List.unzip
                   )
                   pageList
                   |> List.unzip
       in
       ( updatedRowModel, List.concat rowCmds )
-}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
