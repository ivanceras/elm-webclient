module Views.Window.Row exposing (view, viewRowControls)

import Html exposing (..)
import Html.Attributes exposing (type_, attribute, class, classList, href, id, placeholder, src)
import Data.Window.Record as Record exposing (Record,RecordId)
import Data.Window.Value exposing (Value)
import Route exposing (Route)
import Data.Window.Tab as Tab exposing (Tab)
import Data.WindowArena as WindowArena
import Dict
import Views.Window.Value as Value
import Data.Window.Widget exposing (ControlWidget)
import Data.Window.Field as Field exposing (Field)


view: RecordId -> Record -> Tab -> Html msg
view recordId record tab =
    let 
        recordIdString = Record.idToString recordId
        fields = tab.fields -- rearrange fields here if needed
    in
    div [class "tab-row"] 
        (List.map
            (\ field ->
                let 
                    columnName  = Field.columnName field
                    value = Dict.get columnName record
                in
                div [class "tab-row-value"]
                    [Value.viewInList field.controlWidget value]
            )
            fields
        )
    
viewRowControls: RecordId -> Tab -> Html msg
viewRowControls recordId tab =
    div [class "row-controls"]
        [ viewSelectionControl 
        , viewFormLinkControl recordId tab
        , viewEditInPlace
        , viewUndo
        , viewSave
        ] 


viewSelectionControl: Html msg
viewSelectionControl =
    div [ class "row-select"]
        [ input [type_ "checkbox"] []
        ]

viewEditInPlace: Html msg
viewEditInPlace =
    div [ class "edit-in-place"]
        [ div [ class "icon icon-pencil"] []
        ]

viewUndo: Html msg
viewUndo =
    div [ class "row-undo"]
        [ div [ class "icon icon-block"] []
        ]

viewSave: Html msg
viewSave =
    div [ class "row-save"]
        [ div [ class "icon icon-floppy"] []
        ]

viewFormLinkControl: RecordId -> Tab -> Html msg
viewFormLinkControl recordId tab =
    let
        recordIdString = Record.idToString recordId
    in
    a [ class "link-to-form"
      , Route.href (Route.WindowArena (Just (WindowArena.initArgWithRecordId tab.tableName recordIdString))) 
      ]
      [div [class "icon icon-doc-text-inv"]
        []
      ]

