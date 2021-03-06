module Widgets.DropdownDisplay
    exposing
        ( Model
        , Msg(..)
        , getSelected
        , init
        , pageRequestNeeded
        , update
        , view
        )

import Color
import Constant
import Data.Window.Field as Field
import Data.Window.Value as Value exposing (Value)
import Data.Window.Widget as Widget exposing (Alignment)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ionicon
import Util exposing ((=>), Scroll, onScroll, px, viewIf)
import Vendor


type alias Model =
    { opened : Bool
    , selected : Maybe Value
    , scroll : Scroll
    , alignment : Alignment
    , width : Int
    , containerScroll : Scroll
    }


{-|

    return the selected value

-}
getSelected : Model -> Maybe Value
getSelected model =
    model.selected


init : Alignment -> Int -> Maybe Value -> Model
init alignment width selected =
    { opened = False
    , selected = selected
    , scroll = Scroll 0 0
    , alignment = alignment
    , width = width
    , containerScroll = Scroll 0 0
    }


pkCharWidth : List ( Value, a ) -> Int
pkCharWidth list =
    List.map
        (\( pk, display ) ->
            String.length (Value.valueToString pk)
        )
        list
        |> List.maximum
        |> Maybe.withDefault 0


choiceCharWidth : List ( a, String ) -> Int
choiceCharWidth list =
    List.map
        (\( pk, display ) ->
            String.length display
        )
        list
        |> List.maximum
        |> Maybe.withDefault 0


calcPkWidth : List ( Value, a ) -> Int
calcPkWidth list =
    let
        charWidth =
            pkCharWidth list

        ( fontWidth, _ ) =
            Field.fontSize
    in
    charWidth * fontWidth


calcChoiceWidth : List ( a, String ) -> Int
calcChoiceWidth list =
    let
        charWidth =
            choiceCharWidth list

        ( fontWidth, _ ) =
            Field.fontSize
    in
    charWidth * fontWidth


estimatedListHeight : List ( Value, a ) -> Float
estimatedListHeight list =
    let
        optionHeight =
            30.0

        optionLen =
            List.length list
    in
    optionHeight * toFloat optionLen


isScrolledBottom : List ( Value, a ) -> Model -> Bool
isScrolledBottom list model =
    let
        dropdownHeight =
            200

        contentHeight =
            estimatedListHeight list

        scrollTop =
            model.scroll.top

        bottomAllowance =
            100.0
    in
    --Debug.log ("scrollTop(" ++ toString scrollTop ++ ") + model.height(" ++ toString dropdownHeight ++ ") > contentHeight(" ++ toString contentHeight ++ ") - bottomAllowance(" ++ toString bottomAllowance ++ ")")
    scrollTop + dropdownHeight > contentHeight - bottomAllowance


pageRequestNeeded : List ( Value, String ) -> Model -> Bool
pageRequestNeeded list model =
    isScrolledBottom list model


view : List ( Value, String ) -> Model -> Html Msg
view list model =
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
    div [ class "dropdown-display" ]
        [ viewInputButton styles list model
        , viewDropdown styles list model
            |> viewIf model.opened
        ]


viewInputButton : Attribute Msg -> List ( Value, String ) -> Model -> Html Msg
viewInputButton styles list model =
    let
        iconSize =
            14

        iconColor =
            Color.black

        selectedValue =
            case model.selected of
                Just selected ->
                    List.filter
                        (\( pk, choice ) ->
                            pk == selected
                        )
                        list
                        |> List.head

                Nothing ->
                    Nothing

        selectedDisplay =
            case selectedValue of
                Just ( pk, choice ) ->
                    let
                        pkWidth =
                            pkCharWidth list

                        pkString =
                            Value.valueToString pk

                        pkPadded =
                            String.padLeft pkWidth ' ' pkString

                        choiceWidth =
                            choiceCharWidth list

                        choicePadded =
                            String.padLeft choiceWidth ' ' choice
                    in
                    case pk of
                        Value.Uuid _ ->
                            choicePadded ++ "  |   " ++ pkString

                        _ ->
                            pkPadded ++ "  |  " ++ choice

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
            [ Ionicon.arrowDownB iconSize iconColor
            ]
        ]


viewDropdown : Attribute Msg -> List ( Value, String ) -> Model -> Html Msg
viewDropdown styles list model =
    let
        sorted =
            List.sortBy
                (\( pk, display ) ->
                    String.toLower display
                )
                list

        pkWidth =
            calcPkWidth sorted

        choiceWidth =
            calcChoiceWidth sorted

        padTop =
            2

        ( marginTop, marginLeft ) =
            ( -model.containerScroll.top + padTop
            , -model.containerScroll.left
            )
    in
    div
        [ class "dropdown-select"
        , onScroll DropdownScrolled
        , styles

        -- a clever hack for webkit engine, since absolute position in webkit doesn't scroll together
        -- with the container, put the container scroll as margin
        -- using margin since there is no easy way in elm to get the element top.
        , style
            [ ( "margin-top", px marginTop )
            , ( "margin-left", px marginLeft )
            ]
        ]
        [ div [ class "dropdown-options" ]
            (List.map (viewOption ( pkWidth, choiceWidth )) sorted)
        ]


viewOption : ( Int, Int ) -> ( Value, String ) -> Html Msg
viewOption ( pkWidth, choiceWidth ) ( pk, choice ) =
    case pk of
        Value.Uuid _ ->
            viewOptionUuid choiceWidth ( pk, choice )

        _ ->
            viewOptionInt pkWidth ( pk, choice )


viewOptionInt : Int -> ( Value, String ) -> Html Msg
viewOptionInt pkWidth ( pk, choice ) =
    let
        pkString =
            Value.valueToString pk
    in
    div
        [ class "dropdown-option"
        , onMouseDown (SelectionChanged pk)
        ]
        [ div
            [ class "pk-value"
            , style [ ( "min-width", px pkWidth ) ]
            ]
            [ text pkString ]
        , div [ class "choice" ]
            [ text choice ]
        ]


viewOptionUuid : Int -> ( Value, String ) -> Html Msg
viewOptionUuid choiceWidth ( pk, choice ) =
    let
        pkString =
            Value.valueToString pk
    in
    div
        [ class "dropdown-option"
        , onMouseDown (SelectionChanged pk)
        ]
        [ div
            [ class "choice"
            , style [ ( "min-width", px choiceWidth ) ]
            ]
            [ text choice ]
        , div
            [ class "pk-value monospaced"
            ]
            [ text pkString ]
        ]


type Msg
    = ToggleDropdown
    | CloseDropdown
    | SelectionChanged Value
    | DropdownScrolled Scroll
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

        DropdownScrolled scroll ->
            { model | scroll = scroll }
                => Cmd.none

        ContainerScrollChanged scroll ->
            { model
                | containerScroll = scroll
                , opened = False --close the dropdown when scrolled
            }
                => Cmd.none
