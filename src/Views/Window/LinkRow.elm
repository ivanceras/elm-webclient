module Views.Window.LinkRow exposing (..)

import Constant
import Data.Window.Field as Field exposing (Field)
import Data.Window.Lookup as Lookup exposing (Lookup)
import Data.Window.Tab as Tab exposing (Tab)
import Data.Window.TableName as TableName exposing (TableName)
import Data.Window.Value as Value exposing (Value(..))
import Data.Window.Widget as Widget exposing (Alignment(..), DropdownInfo)
import Html exposing (..)
import Html.Attributes exposing (attribute, checked, class, classList, href, id, placeholder, src, style, type_)
import Ionicon
import Util exposing ((=>))
import Views.Window.Field as Field
import Widgets.DropdownDisplay as DropdownDisplay


{-|

    LinkRow displays a LookupTable search to link record in.

-}
type alias Model =
    { tab : Tab
    , dropdownModel : DropdownDisplay.Model
    , dropdownInfo : DropdownInfo
    , linkRowId : Int
    }


getLinkRow : Model -> Maybe Value
getLinkRow model =
    let
        dropdownModel =
            model.dropdownModel

        linkRow =
            DropdownDisplay.getSelected dropdownModel
    in
    linkRow


init : Int -> Tab -> Model
init linkRowId tab =
    let
        widgetWidth =
            200

        alignment =
            Left

        dropdownModel =
            DropdownDisplay.init alignment widgetWidth Nothing

        dropdownInfo =
            { source = tab.tableName
            , display =
                case tab.display of
                    Just display ->
                        display

                    Nothing ->
                        Debug.crash "This tab has no display for linkRow"
            }
    in
    { tab = tab
    , dropdownModel = dropdownModel
    , dropdownInfo = dropdownInfo
    , linkRowId = linkRowId
    }


view : Lookup -> Model -> Html Msg
view lookup model =
    let
        tab =
            model.tab

        display =
            tab.display

        tableName =
            tab.tableName

        ( page, listRecord ) =
            Lookup.tableLookup tableName lookup

        dropdownInfo =
            model.dropdownInfo

        listValue =
            Field.listRecordToListString dropdownInfo listRecord

        iconColor =
            Constant.iconColor

        iconSize =
            Constant.iconSize
    in
    div [ class "link-row" ]
        [ DropdownDisplay.view listValue model.dropdownModel
            |> Html.map DropdownDisplayMsg
        , button []
            [ Ionicon.checkmark iconSize iconColor ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DropdownDisplayMsg msg ->
            let
                ( updatedDropdown, subCmd ) =
                    DropdownDisplay.update msg model.dropdownModel
            in
            { model | dropdownModel = updatedDropdown }
                => Cmd.map DropdownDisplayMsg subCmd


type Msg
    = DropdownDisplayMsg DropdownDisplay.Msg


dropdownPageRequestNeeded : Lookup -> Model -> Maybe TableName
dropdownPageRequestNeeded lookup model =
    let
        dropdownModel =
            model.dropdownModel

        tableName =
            model.tab.tableName

        ( page, listRecord ) =
            Lookup.tableLookup tableName lookup

        dropdownInfo =
            model.dropdownInfo

        listValue =
            Field.listRecordToListString dropdownInfo listRecord
    in
    if
        DropdownDisplay.pageRequestNeeded listValue dropdownModel
            && not (Lookup.hasReachedLastPage tableName lookup)
    then
        Just tableName
    else
        Nothing
