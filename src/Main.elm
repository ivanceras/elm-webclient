port module Main exposing (main)

import Data.DatabaseName as DatabaseName exposing (DatabaseName)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User, Username)
import Data.Window exposing (Slug)
import Data.Window.TableName as TableName exposing (TableName)
import Data.WindowArena as WindowArena
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Navigation exposing (Location)
import Page.Errored as Errored exposing (PageLoadError)
import Page.Login as Login
import Page.NotFound as NotFound
import Page.WindowArena as WindowArena
import Ports
import Request.Auth as Auth
import Route exposing (Route)
import Settings exposing (Settings)
import String.Extra
import Task
import Util exposing ((=>))
import Views.Page as Page exposing (ActivePage)
import Views.Window as Window
import Window as BrowserWindow exposing (Size)


-- WARNING: Based on discussions around how asset management features
-- like code splitting and lazy loading have been shaping up, I expect
-- most of this file to become unnecessary in a future release of Elm.
-- Avoid putting things in here unless there is no alternative!


type Page
    = Blank
    | NotFound
    | Errored PageLoadError
    | WindowArena WindowArena.Model
    | Login Login.Model


type PageState
    = Loaded Page
    | TransitioningFrom Page



-- MODEL --
-- either a user login or a db_url login


type alias Model =
    { session : Session
    , pageState : PageState
    , settings : Settings
    , location : Location
    , browserSize : Size
    }


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    let
        settings =
            Settings.fromJson val

        correctedSettings =
            case settings.apiEndPoint of
                Just apiEndPoint ->
                    settings

                Nothing ->
                    { settings
                        | apiEndPoint =
                            case location.origin of
                                "" ->
                                    Nothing

                                "null" ->
                                    Nothing

                                origin ->
                                    Just origin
                    }

        _ =
            Debug.log "corrected settings: " correctedSettings

        loginRequiredCmd : Cmd Bool
        loginRequiredCmd =
            Auth.loginRequired correctedSettings
                |> Task.attempt
                    (\r ->
                        case r of
                            Ok r ->
                                r

                            Err e ->
                                False
                    )

        dbNameCmd : Cmd (Maybe DatabaseName)
        dbNameCmd =
            Auth.dbName correctedSettings
                |> Task.attempt
                    (\r ->
                        case r of
                            Ok r ->
                                r

                            Err e ->
                                Nothing
                    )

        model =
            { pageState = Loaded initialPage
            , session = { user = decodeUserFromJson val }
            , settings = correctedSettings
            , location = location
            , browserSize = Size 0 0
            }

        setLoginRequiredCmd =
            Cmd.map SetLoginRequired loginRequiredCmd

        setTitleDbNameCmd =
            Cmd.map
                (\dbName ->
                    case dbName of
                        Just dbName ->
                            let
                                ( name, desc ) =
                                    case dbName.description of
                                        Just desc ->
                                            ( dbName.name, desc )

                                        Nothing ->
                                            ( dbName.name, "Diwata" )
                            in
                            SetTitle (Just (String.Extra.toTitleCase name ++ " - " ++ desc))

                        Nothing ->
                            SetTitle Nothing
                )
                dbNameCmd

        setDbNameCmd =
            Cmd.map
                (\dbName ->
                    SetDbName dbName
                )
                dbNameCmd

        getBrowserSize =
            BrowserWindow.size
                |> Task.attempt
                    (\r ->
                        case r of
                            Ok r ->
                                BrowserResized r

                            Err e ->
                                Debug.crash "This should never happen"
                    )
    in
    model
        => Cmd.batch
            [ setLoginRequiredCmd
            , setTitleDbNameCmd
            , setDbNameCmd
            , getBrowserSize
            ]


decodeUserFromJson : Value -> Maybe User
decodeUserFromJson json =
    json
        |> Decode.decodeValue Decode.string
        |> Result.toMaybe
        |> Maybe.andThen (Decode.decodeString User.decoder >> Result.toMaybe)


initialPage : Page
initialPage =
    Blank



-- VIEW --


view : Model -> Html Msg
view model =
    case model.pageState of
        Loaded page ->
            viewPage model.session False page

        TransitioningFrom page ->
            viewPage model.session True page


viewPage : Session -> Bool -> Page -> Html Msg
viewPage session isLoading page =
    let
        frame =
            Page.frame isLoading session.user
    in
    case page of
        NotFound ->
            NotFound.view session
                |> frame Page.Other

        Blank ->
            -- This is for the very initial page load, while we are loading
            -- data via HTTP. We could also render a spinner here.
            Html.text ""
                |> frame Page.Other

        Errored subModel ->
            Errored.view session subModel
                |> frame Page.Other

        WindowArena subModel ->
            WindowArena.view session subModel
                |> frame Page.WindowArena
                |> Html.map WindowArenaMsg

        Login subModel ->
            Login.view session subModel
                |> frame Page.Other
                |> Html.map LoginMsg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pageSubscriptions (getPage model.pageState)
        , Sub.map SetUser sessionChange
        , BrowserWindow.resizes BrowserResized
        ]


sessionChange : Sub (Maybe User)
sessionChange =
    Ports.onSessionChange (Decode.decodeValue User.decoder >> Result.toMaybe)


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page


pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    case page of
        Blank ->
            Sub.none

        Errored _ ->
            Sub.none

        NotFound ->
            Sub.none

        WindowArena windowArenaModel ->
            Sub.map WindowArenaMsg (WindowArena.subscriptions windowArenaModel)

        Login _ ->
            Sub.none



-- UPDATE --


type Msg
    = SetRoute (Maybe Route)
    | HomeLoaded (Result PageLoadError WindowArena.Model)
    | WindowArenaMsg WindowArena.Msg
    | SetUser (Maybe User)
    | SetTitle (Maybe String)
    | SetDbName (Maybe DatabaseName)
    | LoginMsg Login.Msg
    | SetLoginRequired Bool
    | BrowserResized Size


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        _ =
            Debug.log "setting route" maybeRoute

        settings =
            model.settings

        prevPage =
            getPage model.pageState

        updatedPrevPage =
            case prevPage of
                WindowArena windowArena ->
                    WindowArena { windowArena | loadingSelectedRecord = True }

                _ ->
                    prevPage

        transition toMsg task =
            { model | pageState = TransitioningFrom updatedPrevPage }
                => Task.attempt toMsg task

        errored =
            pageErrored model

        _ =
            Debug.log "cred" settings.cred

        correctedRoute =
            if settings.loginRequired then
                case settings.cred of
                    Just cred ->
                        maybeRoute

                    Nothing ->
                        Just Route.Login
            else
                maybeRoute
    in
    case correctedRoute of
        Nothing ->
            { model | pageState = Loaded NotFound } => Cmd.none

        Just (Route.WindowArena arenaArg) ->
            transition HomeLoaded (WindowArena.init model.settings model.session arenaArg model.browserSize)

        Just Route.Login ->
            { model | pageState = Loaded (Login (Login.initialModel settings)) } => Cmd.none

        Just Route.Logout ->
            let
                session =
                    model.session
            in
            { model | session = { session | user = Nothing } }
                => Cmd.batch
                    [ Ports.storeSession Nothing
                    , Route.modifyUrl (Route.WindowArena WindowArena.default)
                    ]


pageErrored : Model -> ActivePage -> String -> ( Model, Cmd msg )
pageErrored model activePage errorMessage =
    let
        error =
            Errored.pageLoadError activePage errorMessage
    in
    { model | pageState = Loaded (Errored error) } => Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        session =
            model.session

        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
            ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )

        errored =
            pageErrored model
    in
    case ( msg, page ) of
        ( SetRoute route, WindowArena arenaModel ) ->
            setRoute route model

        ( SetRoute route, _ ) ->
            setRoute route model

        ( HomeLoaded (Ok subModel), _ ) ->
            let
                _ =
                    Debug.log "WindowArena is now loaded" ""
            in
            { model | pageState = Loaded (WindowArena subModel) } => Cmd.none

        ( HomeLoaded (Err error), _ ) ->
            { model | pageState = Loaded (Errored error) } => Cmd.none

        ( SetUser user, _ ) ->
            let
                session =
                    model.session

                cmd =
                    -- If we just signed out, then redirect to WindowArena.
                    if session.user /= Nothing && user == Nothing then
                        Route.modifyUrl (Route.WindowArena WindowArena.default)
                    else
                        Cmd.none
            in
            { model | session = { session | user = user } }
                => cmd

        ( SetTitle text, _ ) ->
            model
                => (case text of
                        Just text ->
                            title text

                        Nothing ->
                            Cmd.none
                   )

        ( SetDbName dbName, _ ) ->
            { model | settings = Settings.setDbName model.settings dbName }
                => Cmd.none

        ( LoginMsg subMsg, Login subModel ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    Login.update subMsg subModel

                ( newModel, subCmd ) =
                    case msgFromPage of
                        Login.NoOp ->
                            ( model, Cmd.none )

                        Login.SetSettings settings ->
                            ( { model | settings = settings }
                            , portCredentials settings
                            )
            in
            { newModel | pageState = Loaded (Login pageModel) }
                => Cmd.batch
                    [ Cmd.map LoginMsg cmd
                    , subCmd
                    ]

        ( WindowArenaMsg (WindowArena.ToggleWindowList isWindowListHidden), WindowArena subModel ) ->
            let
                updatedSettings =
                    model.settings

                updatedModel =
                    { model
                        | settings = { updatedSettings | isWindowListHidden = isWindowListHidden }
                    }

                subMsg =
                    WindowArena.ToggleWindowList isWindowListHidden

                _ =
                    Debug.log "updated model settings: " updatedModel.settings

                ( updatedWindowArena, subCmd ) =
                    WindowArena.update session (WindowArena.SetSettings updatedModel.settings) subModel

                ( updatedWindowArena2, newCmd ) =
                    WindowArena.update session subMsg updatedWindowArena
            in
            ( { model | pageState = Loaded (WindowArena updatedWindowArena2) }
            , Cmd.batch
                [ Cmd.map WindowArenaMsg newCmd
                , setWindowListIsHidden updatedModel.settings.isWindowListHidden
                ]
            )

        ( WindowArenaMsg subMsg, WindowArena subModel ) ->
            toPage WindowArena WindowArenaMsg (WindowArena.update session) subMsg subModel

        ( SetLoginRequired isRequired, _ ) ->
            let
                updatedModel =
                    { model
                        | settings = Settings.setDbUrl model.settings isRequired
                    }
            in
            setRoute (Route.fromLocation updatedModel.location) updatedModel

        ( BrowserResized size, WindowArena subModel ) ->
            let
                ( updatedModel2, subCmd ) =
                    toPage WindowArena WindowArenaMsg (WindowArena.update session) (WindowArena.BrowserResized size) subModel
            in
            { updatedModel2 | browserSize = size }
                => subCmd

        ( BrowserResized size, _ ) ->
            { model | browserSize = size }
                => Cmd.none

        ( _, NotFound ) ->
            -- Disregard incoming messages when we're on the
            -- NotFound page.
            model => Cmd.none

        ( _, _ ) ->
            -- Disregard incoming messages that arrived for the wrong page
            model => Cmd.none


portCredentials : Settings -> Cmd a
portCredentials settings =
    case settings.cred of
        Just cred ->
            Cmd.batch
                [ setUsername cred.username
                , setPassword cred.password
                ]

        Nothing ->
            Cmd.none



-- MAIN --


main : Program Value Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


port title : String -> Cmd a


port setUsername : String -> Cmd a


port setPassword : String -> Cmd a


port setWindowListIsHidden : Bool -> Cmd a
