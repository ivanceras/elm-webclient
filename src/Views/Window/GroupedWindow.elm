module Views.Window.GroupedWindow
    exposing
        ( Model
        , Msg(..)
        , init
        , update
        , view
        , viewWindowNames
        )

{-| The reusable Window Feed that appears on both the Home page as well as on
the Profile page. There's a lot of logic here, so it's more convenient to use
the heavyweight approach of giving this its own Model, view, and update.

This means callers must use Html.map and Cmd.map to use this thing, but in
this case that's totally worth it because of the amount of logic wrapped up
in this thing.

For every other reusable view in this application, this API would be totally
overkill, so we use simpler APIs instead.

-}

import Color
import Constant
import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (Username)
import Data.Window as Window exposing (Tag, Window)
import Data.Window.GroupedWindow as GroupedWindow exposing (GroupedWindow, WindowName)
import Data.Window.TableName as TableName exposing (TableName, tableNameToString)
import Data.WindowArena as WindowArena
import Dom.Scroll
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Ionicon
import Material.Icons.Action as MaterialAction
import Request.Window
import Route exposing (Route)
import SelectList exposing (Position(..), SelectList)
import Settings exposing (Settings)
import Task exposing (Task)
import Util exposing ((=>), onClickStopPropagation, pair, px, viewIf)
import Views.Errors as Errors
import Views.Page exposing (bodyId)


-- MODEL --


type alias Model =
    { errors : List String
    , groupedWindow : List GroupedWindow
    , filteredWindow : List GroupedWindow
    , activeWindow : Maybe TableName
    , isLoading : Bool
    , windowSearch : Maybe String
    , isHidden : Bool
    }


init : Settings -> Maybe TableName -> Task Http.Error Model
init settings activeWindow =
    let
        toModel ( activeWindow, groupedWindow ) =
            { errors = []
            , activeWindow = activeWindow
            , groupedWindow = groupedWindow
            , filteredWindow = groupedWindow
            , isLoading = False
            , windowSearch = Nothing
            , isHidden = settings.isWindowListHidden
            }
    in
    fetch settings activeWindow
        |> Task.map toModel



-- VIEW --


view : Model -> Html Msg
view model =
    div
        [ class "pane pane-sm sidebar grouped-window-list animated"
        , classList [ ( "hide", model.isHidden ) ]
        ]
        [ div [ class "grouped-window" ]
            (textSearch model
                :: viewWindowNames model
            )
        ]


textSearch : Model -> Html Msg
textSearch model =
    let
        iconColor =
            Constant.iconColor

        iconSize =
            Constant.columnSearchIconSize

        windowSearch =
            case model.windowSearch of
                Just v ->
                    v

                Nothing ->
                    ""
    in
    div [ class "window-filter" ]
        [ div [ class "filter-icon-wrapper" ]
            [ div [ class "fa filter-value-icon" ]
                [ Ionicon.search iconSize iconColor ]
            ]
        , input
            [ class "filter-value"
            , type_ "text" -- "search" will render badly in webkit-gtk
            , value windowSearch
            , onInput SearchValueChanged
            ]
            []
        ]


viewWindowName : Maybe TableName -> WindowName -> Html msg
viewWindowName activeWindow windowName =
    let
        isActive =
            case activeWindow of
                Just tableName ->
                    windowName.tableName == tableName

                Nothing ->
                    False

        isView =
            windowName.isView

        iconColor =
            if isView then
                Constant.viewIconColor
            else
                Constant.iconColor

        iconSize =
            Constant.iconSize
    in
    a
        [ class "nav-group-item"
        , classList [ ( "active", isActive ), ( "is-view-active", isView && isActive ) ]
        , Route.href (Route.WindowArena (WindowArena.initArg (Just windowName.tableName)))
        ]
        [ span
            [ class "table-icon"
            , classList [ ( "is-view-icon", isView ) ]
            ]
            [ MaterialAction.list iconColor iconSize ]
        , text windowName.name
        ]


viewWindowGroup : Maybe TableName -> GroupedWindow -> Html msg
viewWindowGroup activeWindow groupedWindow =
    nav [ class "nav-group" ]
        [ h5 [ class "nav-group-title" ] [ text groupedWindow.group ]
        , div [] <| List.map (viewWindowName activeWindow) groupedWindow.windowNames
        ]


viewWindowNames : Model -> List (Html Msg)
viewWindowNames model =
    List.map (viewWindowGroup model.activeWindow) model.filteredWindow



-- UPDATE --


type Msg
    = DismissErrors
    | SearchValueChanged String
    | SetVisibility Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DismissErrors ->
            { model | errors = [] } => Cmd.none

        SearchValueChanged value ->
            let
                updatedModel =
                    { model
                        | windowSearch =
                            case value of
                                "" ->
                                    Nothing

                                v ->
                                    Just v
                    }
            in
            updateWindowSearch updatedModel
                => Cmd.none

        SetVisibility isHidden ->
            { model | isHidden = isHidden }
                => Cmd.none


updateWindowSearch : Model -> Model
updateWindowSearch model =
    let
        filteredWindow =
            case model.windowSearch of
                Just search ->
                    List.map (GroupedWindow.findMatch search) model.groupedWindow

                Nothing ->
                    model.groupedWindow
    in
    { model
        | filteredWindow = filteredWindow
    }


fetch : Settings -> Maybe TableName -> Task Http.Error ( Maybe TableName, List GroupedWindow )
fetch settings activeWindow =
    Request.Window.list settings
        |> Http.toTask
        |> Task.map (\groupedWindow -> ( activeWindow, groupedWindow ))
