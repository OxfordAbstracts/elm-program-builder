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
import Html.Events exposing (on)


manageInformationWarning model =
    let
        blankFileTitleInput =
            model.infoToSave
                |> List.map .infoTitle
                |> List.any (\n -> n == "")

        blankFileContents =
            model.infoToSave
                |> List.map .contents
                |> List.any (\c -> c == "")
    in
        if model.showManageInformationUi && blankFileTitleInput then
            getWarning "File title field cannot be empty" model
        else
            ""


view : Model -> Html Msg
view model =
    let
        displayDiv =
            if (not model.showManageInformationUi) then
                "none"
            else
                "block"

        savedInfoDivs =
            List.indexedMap
                (\i f ->
                    div []
                        [ div [ class "form__question-sub-section form__question-sub-section--table" ]
                            [ input
                                [ class "form__input"
                                , type_ "text"
                                , placeholder "Title *"
                                , value f.infoTitle
                                , onInput (ChangeSavedInfoTitle f.id)
                                ]
                                [ text f.infoTitle ]
                            ]
                        , div [ class "form__question-sub-section form__question-sub-section--table" ]
                            [ input
                                [ class "form__input"
                                , type_ "text"
                                , placeholder "Description"
                                , value f.infoDescription
                                , onInput (ChangeSavedInfoDescription f.id)
                                ]
                                [ text f.infoDescription ]
                            ]
                        , div [ class "form__question-sub-section form__question-sub-section--table" ]
                            [ a [ href f.filelink ] [ text f.filename ] ]
                        , div [ class "form__question-sub-section form__question-sub-section--table form__question-sub-section--button" ]
                            [ button
                                [ onClick (ConfirmDeleteInformation f.id)
                                , class "button button--secondary icon icon--bin"
                                ]
                                []
                            ]
                        , div [ class "constructor__question-controls" ]
                            [ button
                                [ class "button button--glass icon icon--up-open"
                                , onClick (MoveInfoUp i)
                                , disabled (i == 0)
                                ]
                                []
                            , button
                                [ class "button button--glass icon icon--down-open"
                                , onClick (MoveInfoDown i)
                                , disabled (i == ((List.length model.savedInfo) - 1))
                                ]
                                []
                            ]
                        ]
                )
                model.savedInfo

        infoToSaveDivs =
            model.infoToSave
                |> List.map
                    (\f ->
                        div []
                            [ div [ class "form__question-sub-section form__question-sub-section--table" ]
                                [ input
                                    [ class "form__input"
                                    , type_ "text"
                                    , placeholder "Title *"
                                    , id ("info-title-" ++ (toString f.id))
                                    , onInput (ChangeInfoToSaveTitle f.id)
                                    ]
                                    []
                                ]
                            , div [ class "form__question-sub-section form__question-sub-section--table" ]
                                [ input
                                    [ class "form__input"
                                    , type_ "text"
                                    , placeholder "Description"
                                    , id ("info-description-" ++ (toString f.id))
                                    , onInput (ChangeInfoToSaveDescription f.id)
                                    ]
                                    []
                                ]
                            , div [ class "form__question-sub-section form__question-sub-section--table" ]
                                [ input
                                    [ id ("file-to-save-" ++ (toString f.id))
                                    , type_ "file"
                                    , on "change"
                                        (Json.Decode.succeed (FileSelected f.id))
                                    ]
                                    []
                                ]
                            , div [ class "form__question-sub-section form__question-sub-section--table form__question-sub-section--button" ]
                                [ button
                                    [ onClick (DeleteInfoToSave f.id)
                                    , class "button button--secondary icon icon--bin"
                                    ]
                                    []
                                ]
                            ]
                    )

        displayWarning =
            not (String.isEmpty (manageInformationWarning model))

        onClickUpdate =
            if String.isEmpty (manageInformationWarning model) then
                SaveInfo
            else
                ShowValidationMessage
    in
        div [ class "form form--add-to-view", style [ ( "display", displayDiv ) ] ]
            [ div [] savedInfoDivs
            , div [] infoToSaveDivs
            , div []
                [ if model.showSavingFilesSpinner then
                    div [ class "loader" ] []
                  else if displayWarning && model.showValidation then
                    div [ class "form__hint form__hint--warning" ]
                        [ text (manageInformationWarning model) ]
                  else
                    div []
                        [ button
                            [ class "button button--tertiary button--wider"
                            , type_ "button"
                            , onClick AddNewInformation
                            ]
                            [ text "Add New Information" ]
                        , button [ class "button button--primary button--wider", type_ "button", onClick onClickUpdate ]
                            [ text "Save" ]
                        ]
                ]
            ]
