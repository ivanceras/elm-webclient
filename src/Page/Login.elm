module Page.Login exposing (ExternalMsg(..), Model, Msg, initialModel, update, view)

{-| The login page.
-}

import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Data.WindowArena as WindowArena
import Html exposing (..)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional)
import Request.Auth as Auth
import Route exposing (Route)
import Settings exposing (Settings)
import Util exposing ((=>))
import Validate exposing (..)
import Views.Form as Form


-- MODEL --


type alias Model =
    { errors : List Error
    , username : String
    , password : String
    , settings : Settings
    }


initialModel : Settings -> Model
initialModel settings =
    { errors = []
    , username = ""
    , password = ""
    , settings = settings
    }



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    div [ class "auth-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                    [ h1 [ class "text-xs-center" ] [ text "Sign in" ]
                    , Form.viewErrors model.errors
                    , viewForm
                    ]
                ]
            ]
        ]


viewForm : Html Msg
viewForm =
    Html.form [ onSubmit SubmitForm ]
        [ Form.input
            [ class "form-control-lg"
            , placeholder "username"
            , onInput SetUsername
            ]
            []
        , Form.password
            [ class "form-control-lg"
            , placeholder "password"
            , onInput SetPassword
            ]
            []
        , button [ class "btn btn-lg btn-primary pull-xs-right" ]
            [ text "Connect" ]
        ]



-- UPDATE --


type Msg
    = SubmitForm
    | SetUsername String
    | SetPassword String
    | LoginCompleted (Result Http.Error Bool)


type ExternalMsg
    = NoOp
    | SetSettings Settings


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        SubmitForm ->
            case validate model of
                [] ->
                    { model | errors = [] }
                        => Http.send LoginCompleted (Auth.login model.settings)
                        => NoOp

                errors ->
                    { model | errors = errors }
                        => Cmd.none
                        => NoOp

        SetUsername username ->
            let
                updatedModel =
                    { model
                        | username = username
                        , settings = Settings.setUsername model.settings username
                    }
            in
            updatedModel
                => Cmd.none
                => NoOp

        SetPassword password ->
            let
                updatedModel =
                    { model
                        | password = password
                        , settings = Settings.setPassword model.settings password
                    }
            in
            updatedModel
                => Cmd.none
                => NoOp

        LoginCompleted (Err error) ->
            let
                errorMessages =
                    case error of
                        Http.BadStatus response ->
                            let
                                _ =
                                    Debug.log "Its a bad status"
                            in
                            response.body

                        _ ->
                            "unable to process registration"
            in
            { model | errors = [ Form => errorMessages ] }
                => Cmd.none
                => NoOp

        LoginCompleted (Ok v) ->
            model
                => Cmd.batch [ Route.modifyUrl (Route.WindowArena WindowArena.default) ]
                => SetSettings model.settings



-- VALIDATION --


type Field
    = Form
    | Username
    | Password


{-| Recording validation errors on a per-field basis facilitates displaying
them inline next to the field where the error occurred.

I implemented it this way out of habit, then realized the spec called for
displaying all the errors at the top. I thought about simplifying it, but then
figured it'd be useful to show how I would normally model this data - assuming
the intended UX was to render errors per field.

(The other part of this is having a view function like this:

viewFormErrors : Field -> List Error -> Html msg

...and it filters the list of errors to render only the ones for the given
Field. This way you can call this:

viewFormErrors Email model.errors

...next to the `dbUrl` field, and call `viewFormErrors Password model.errors`
next to the `password` field, and so on.

-}
type alias Error =
    ( Field, String )


validate : Model -> List Error
validate =
    Validate.all
        [ .username >> ifBlank (Username => "username can't be blank.")
        , .password >> ifBlank (Password => "password can't be blank.")
        ]


errorsDecoder : Decoder (List String)
errorsDecoder =
    decode (\dbUrl -> List.concat [ dbUrl ])
        |> optionalError "dbUrl"


optionalError : String -> Decoder (List String -> a) -> Decoder a
optionalError fieldName =
    let
        errorToString errorMessage =
            String.join " " [ fieldName, errorMessage ]
    in
    optional fieldName (Decode.list (Decode.map errorToString string)) []
