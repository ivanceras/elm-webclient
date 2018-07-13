module Widgets.FixDropdown exposing (Model, Msg(..), init, update, view)

import Data.Window.Field as Field
import Data.Window.Widget as Widget exposing (Alignment)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Util exposing ((=>), Scroll, onScroll, px, viewIf)
import Vendor


type alias Model =
    { opened : Bool
    , list : List String
    , selected : Maybe String
    , alignment : Alignment
    , width : Int
    , containerScroll : Scroll
    }


init : Alignment -> Int -> Maybe String -> List String -> Model
init alignment width selected list =
    { opened = False
    , list = list
    , selected = selected
    , alignment = alignment
    , width = width
    , containerScroll = Scroll 0 0
    }


view : Model -> Html Msg
view model =
    let
        alignment =
            model.alignment

        alignmentString =
            Widget.alignmentToString alignment

        widgetWidth =
            model.width - 10

        styles =
            style
                [ ( "text-align", alignmentString )
                , ( "width", px widgetWidth )
                ]
    in
    div []
        [ viewInputButton styles model
        , viewIf model.opened (viewDropdown styles model)
        ]


viewInputButton : Attribute Msg -> Model -> Html Msg
viewInputButton styles model =
    let
        selectedValue =
            case model.selected of
                Just selected ->
                    List.filter
                        (\choice ->
                            choice == selected
                        )
                        model.list
                        |> List.head

                Nothing ->
                    Nothing

        selectedDisplay =
            case selectedValue of
                Just choice ->
                    choice

                Nothing ->
                    ""
    in
    div [ class "dropdown-input" ]
        [ input
            [ onClick ToggleDropdown
            , onBlur CloseDropdown
            , value selectedDisplay
            , styles
            ]
            []
        , button
            [ onClick ToggleDropdown
            , onBlur CloseDropdown
            ]
            [ i [ class "fa fa-caret-down" ] []
            ]
        ]


viewDropdown : Attribute Msg -> Model -> Html Msg
viewDropdown styles model =
    let
        sorted =
            List.sortBy String.toLower model.list

        padTop =
            2

        ( marginTop, marginLeft ) =
            ( -model.containerScroll.top + padTop
            , -model.containerScroll.left
            )
    in
    div
        [ class "dropdown-select"
        , styles
        , style
            [ ( "margin-top", px marginTop )
            , ( "margin-left", px marginLeft )
            ]
        ]
        [ div [ class "dropdown-options" ]
            (List.map viewOption sorted)
        ]


viewOption : String -> Html Msg
viewOption choice =
    div
        [ class "dropdown-option"
        , onMouseDown (SelectionChanged choice)
        ]
        [ div [ class "choice" ]
            [ text choice ]
        ]


type Msg
    = ToggleDropdown
    | CloseDropdown
    | SelectionChanged String
    | ContainerScrollChanged Scroll


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleDropdown ->
            { model | opened = not model.opened }
                => Cmd.none

        CloseDropdown ->
            { model | opened = False }
                => Cmd.none

        SelectionChanged selected ->
            let
                newModel =
                    { model | selected = Just selected }
            in
            update CloseDropdown newModel

        ContainerScrollChanged scroll ->
            { model
                | containerScroll = scroll
                , opened = False --close the dropdown when scrolled
            }
                => Cmd.none
