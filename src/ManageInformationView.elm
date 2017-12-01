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
            model.filesToSave
                |> List.map .filetitle
                |> List.any (\n -> n == "")

        blankFileContents =
            model.filesToSave
                |> List.map .contents
                |> List.any (\c -> c == "")
    in
        if model.showManageInformationUi && blankFileTitleInput then
            getWarning "File title field cannot be empty" model
        else if model.showManageInformationUi && blankFileContents then
            getWarning "Please upload a file with all information" model
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

        savedFilesDivs =
            List.indexedMap
                (\i f ->
                    div []
                        [ div [ class "form__question-sub-section form__question-sub-section--table" ]
                            [ input
                                [ class "form__input"
                                , type_ "text"
                                , placeholder "File title"
                                , value f.filetitle
                                , onInput (ChangeSavedFileTitle f.id)
                                ]
                                [ text f.filetitle ]
                            ]
                        , div [ class "form__question-sub-section form__question-sub-section--table" ]
                            [ input
                                [ class "form__input"
                                , type_ "text"
                                , placeholder "File description"
                                , value f.filedescription
                                , onInput (ChangeSavedFileDescription f.id)
                                ]
                                [ text f.filedescription ]
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
                                , onClick (MoveFileUp i)
                                , disabled (i == 0)
                                ]
                                []
                            , button
                                [ class "button button--glass icon icon--down-open"
                                , onClick (MoveFileDown i)
                                , disabled (i == ((List.length model.savedFiles) - 1))
                                ]
                                []
                            ]
                        ]
                )
                model.savedFiles

        filesToSaveDivs =
            model.filesToSave
                |> List.map
                    (\f ->
                        div []
                            [ div [ class "form__question-sub-section form__question-sub-section--table" ]
                                [ input
                                    [ class "form__input"
                                    , type_ "text"
                                    , placeholder "File title"
                                    , id ("file-title-" ++ (toString f.id))
                                    , onInput (ChangeFileToSaveTitle f.id)
                                    ]
                                    []
                                ]
                            , input
                                [ class "form__input"
                                , type_ "text"
                                , placeholder "File description"
                                , id ("file-description-" ++ (toString f.id))
                                , onInput (ChangeFileToSaveDescription f.id)
                                ]
                                []
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
                                    [ onClick (DeleteFileToSave f.id)
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
                SaveFiles
            else
                ShowValidationMessage
    in
        div [ class "form form--add-to-view", style [ ( "display", displayDiv ) ] ]
            [ div [] savedFilesDivs
            , div [] filesToSaveDivs
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
