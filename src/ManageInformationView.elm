module ManageInformationView exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import MainMessages exposing (..)
import MainModel exposing (..)
import MainMessages exposing (..)
import GetWarning exposing (..)
import Json.Decode
import Json.Decode.Pipeline exposing (required, optional, decode)


-- import Ports exposing (FilePortData, fileSelected, fileContentRead)

import Html.Events exposing (on)


view : Model -> Html Msg
view model =
    let
        displayDiv =
            if (not model.showManageInformationUi) then
                "none"
            else
                "block"

        savedInformationDivs =
            List.map (\f -> div [] [ a [ href f.filelink ] [ text f.filetitle ] ]) model.savedFiles
    in
        div [ class "form form--add-to-view", style [ ( "display", displayDiv ) ] ]
            [ div [] savedInformationDivs
            , div [ class "form__question-sub-section form__question-sub-section--table" ]
                [ input
                    [ class "form__input"
                    , type_ "text"
                    , placeholder "File title"
                    , id "file-title-1"
                      -- TODO: 1 SHOULD BE INDEX INPUT
                      -- , onInput (UpdateFileTitles 0)
                    ]
                    []
                ]
            , div [ class "form__question-sub-section form__question-sub-section--table" ]
                -- [ label [ class "button button--tertiary ", for "1" ]
                --     [ text "Choose file" ]
                [ input
                    [ -- class "form__input form__input--file file-inputs"
                      id "1"
                    , type_ "file"
                    , on "change"
                        (Json.Decode.succeed FileSelected)
                    ]
                    []
                ]
            , div [ class "bar bar--button" ]
                [ button [ class "button button--primary button--wider", type_ "button", onClick SaveFiles ]
                    [ text "Save" ]
                ]
            ]



-- div []
--     [ label [ class "button button--tertiary form__label--file" ]
--         [ text "Choose file" ]
--     , input
--         [ class "form__input form__input--file file-inputs"
--           -- class "form__input"
--           -- ,
--         , type_ "file"
--           -- , value t.name
--           -- , onInput (UpdatePickedTrack t.id Name)
--         ]
--         []
--     ]
