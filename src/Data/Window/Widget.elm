module Data.Window.Widget
    exposing
        ( Alignment(..)
        , ControlWidget
        , Dropdown(..)
        , DropdownInfo
        , Widget(..)
        , alignmentToString
        , controlWidgetDecoder
        , decoder
        )

import Data.Window.Display as Display exposing (IdentifierDisplay)
import Data.Window.TableName as TableName exposing (TableName)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra
import Json.Decode.Pipeline as Pipeline exposing (decode, required)


type alias ControlWidget =
    { widget : Widget
    , dropdown : Maybe Dropdown
    , width : Int
    , maxLen : Maybe Int
    , height : Int
    , alignment : Alignment
    }


type Dropdown
    = TableDropdown DropdownInfo


type alias DropdownInfo =
    { source : TableName
    , display : IdentifierDisplay
    }


type Alignment
    = Left
    | Right
    | Center


alignmentToString : Alignment -> String
alignmentToString alignment =
    case alignment of
        Left ->
            "left"

        Right ->
            "right"

        Center ->
            "center"


type Widget
    = Textbox
    | IntegerTextbox
    | DecimalTextbox
    | UuidTextbox
    | Password
    | TagSelection
    | MultilineText
    | MarkdownHtml
    | CodeHighligher
    | ColorSelection
    | DatePicker
    | DateTimePicker
    | LogoImage
    | MediumImage
    | LargeImageEmbed
    | FixDropdown (List String)
    | Radiogroup (List String)
    | Checkboxgroup (List String)
    | Dropdown
    | DropdownWithImage
    | AutocompleteDropdown
    | DialogDropdown
    | TableLookupDropdown
    | Checkbox
    | CheckmarkStatusImage
    | IndicatorStatusImage
    | ToggleButton
    | UrlLink
    | PrimaryUrlLink
    | UrlTextbox
    | VideoLink
    | YoutubeVideoEmbed
    | TweetEmbed
    | PrimaryButton
    | SecondaryButton
    | AuxillaryButton
    | FileDownloadLink
    | FileUpload
    | MapLookup
    | CountryList
    | CountryListWithFlag
    | TimezoneLookup
    | PdfViewer
    | ExcelViewer
    | CsvRenderer
    | VideoPlayer
    | AudioPlayer
    | Viewer3D


decoder : Decoder Widget
decoder =
    Decode.oneOf
        [ simpleDecoder
        , dropDownWidgetDecoder
        , radiogroupWidgetDecoder
        ]


simpleDecoder : Decoder Widget
simpleDecoder =
    Decode.string
        |> Decode.andThen
            (\val ->
                case val of
                    "Textbox" ->
                        Decode.succeed Textbox

                    "IntegerTextbox" ->
                        Decode.succeed IntegerTextbox

                    "DecimalTextbox" ->
                        Decode.succeed DecimalTextbox

                    "UuidTextbox" ->
                        Decode.succeed UuidTextbox

                    "Password" ->
                        Decode.succeed Password

                    "TagSelection" ->
                        Decode.succeed TagSelection

                    "MultilineText" ->
                        Decode.succeed MultilineText

                    "MarkdownHtml" ->
                        Decode.succeed MarkdownHtml

                    "CodeHighligher" ->
                        Decode.succeed CodeHighligher

                    "ColorSelection" ->
                        Decode.succeed ColorSelection

                    "DatePicker" ->
                        Decode.succeed DatePicker

                    "DateTimePicker" ->
                        Decode.succeed DateTimePicker

                    "LogoImage" ->
                        Decode.succeed LogoImage

                    "MediumImage" ->
                        Decode.succeed MediumImage

                    "LargeImageEmbed" ->
                        Decode.succeed LargeImageEmbed

                    "Dropdown" ->
                        Decode.succeed Dropdown

                    "DropdownWithImage" ->
                        Decode.succeed DropdownWithImage

                    "AutocompleteDropdown" ->
                        Decode.succeed AutocompleteDropdown

                    "DialogDropdown" ->
                        Decode.succeed DialogDropdown

                    "TableLookupDropdown" ->
                        Decode.succeed TableLookupDropdown

                    "Checkbox" ->
                        Decode.succeed Checkbox

                    "CheckmarkStatusImage" ->
                        Decode.succeed CheckmarkStatusImage

                    "IndicatorStatusImage" ->
                        Decode.succeed IndicatorStatusImage

                    "ToggleButton" ->
                        Decode.succeed ToggleButton

                    "UrlLink" ->
                        Decode.succeed UrlLink

                    "PrimaryUrlLink" ->
                        Decode.succeed PrimaryUrlLink

                    "UrlTextbox" ->
                        Decode.succeed UrlTextbox

                    "VideoLink" ->
                        Decode.succeed VideoLink

                    "YoutubeVideoEmbed" ->
                        Decode.succeed YoutubeVideoEmbed

                    "TweetEmbed" ->
                        Decode.succeed TweetEmbed

                    "PrimaryButton" ->
                        Decode.succeed PrimaryButton

                    "SecondaryButton" ->
                        Decode.succeed SecondaryButton

                    "AuxillaryButton" ->
                        Decode.succeed AuxillaryButton

                    "FileDownloadLink" ->
                        Decode.succeed FileDownloadLink

                    "FileUpload" ->
                        Decode.succeed FileUpload

                    "MapLookup" ->
                        Decode.succeed MapLookup

                    "CountryList" ->
                        Decode.succeed CountryList

                    "CountryListWithFlag" ->
                        Decode.succeed CountryListWithFlag

                    "TimezoneLookup" ->
                        Decode.succeed TimezoneLookup

                    "PdfViewer" ->
                        Decode.succeed PdfViewer

                    "ExcelViewer" ->
                        Decode.succeed ExcelViewer

                    "CsvRenderer" ->
                        Decode.succeed CsvRenderer

                    "VideoPlayer" ->
                        Decode.succeed VideoPlayer

                    "AudioPlayer" ->
                        Decode.succeed AudioPlayer

                    "Viewer3D" ->
                        Decode.succeed Viewer3D

                    _ ->
                        Decode.fail ("not yet dealth with widget: " ++ val)
            )


controlWidgetDecoder : Decoder ControlWidget
controlWidgetDecoder =
    decode ControlWidget
        |> required "widget" decoder
        |> required "dropdown" (Decode.nullable dropdownDecoder)
        |> required "width" Decode.int
        |> required "max_len" (Decode.nullable Decode.int)
        |> required "height" Decode.int
        |> required "alignment" alignmentDecoder


dropdownDecoder : Decoder Dropdown
dropdownDecoder =
    Decode.oneOf
        [ tableDropdownDecoder
        ]


tableDropdownDecoder : Decoder Dropdown
tableDropdownDecoder =
    decode TableDropdown
        |> required "TableDropdown" dropdownInfoDecoder


dropdownInfoDecoder : Decoder DropdownInfo
dropdownInfoDecoder =
    decode DropdownInfo
        |> required "source" TableName.decoder
        |> required "display" Display.decoder


alignmentDecoder : Decoder Alignment
alignmentDecoder =
    Decode.string
        |> Decode.andThen
            (\val ->
                case val of
                    "Left" ->
                        Decode.succeed Left

                    "Right" ->
                        Decode.succeed Right

                    "Center" ->
                        Decode.succeed Center

                    _ ->
                        Decode.fail ("Expecting Left, Right, Center, found: " ++ val)
            )


dropDownWidgetDecoder : Decoder Widget
dropDownWidgetDecoder =
    decode FixDropdown
        |> required "FixDropdown" (Decode.list Decode.string)


radiogroupWidgetDecoder : Decoder Widget
radiogroupWidgetDecoder =
    decode Radiogroup
        |> required "Radiogroup" (Decode.list Decode.string)
