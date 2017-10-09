module NewTrackView exposing (view, newTrackWarning)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import MainMessages exposing (..)
import MainModel exposing (..)
import MainMessages exposing (..)
import GetWarning exposing (..)


newTrackWarning model =
    let
        blankPickedTrackNameInput =
            model.pickedTracks
                |> List.map .name
                |> List.any (\n -> n == "")

        blankPickedTrackDescriptionInput =
            model.pickedTracks
                |> List.map .description
                |> List.any (\n -> n == "")
    in
        if model.showNewTrackUi && (blankPickedTrackNameInput || blankPickedTrackDescriptionInput) then
            getWarning "Track name and description fields cannot be empty" model
        else
            ""


view : Model -> Html Msg
view model =
    let
        toStringIgnore0 int =
            if int == 0 then
                ""
            else
                toString int

        disableInput trackId =
            let
                tracksWithSessions =
                    model.datesWithSessions
                        |> List.concatMap .sessions
                        |> List.map .trackId
            in
                if List.member (Just trackId) tracksWithSessions then
                    disabled True
                else
                    disabled False

        onClickUpdate =
            if String.isEmpty (newTrackWarning model) then
                UpdateTracks
            else
                ShowValidationMessage

        validationWarningDiv =
            if model.showValidation then
                div [ class "form__hint form__hint--warning" ]
                    [ text (newTrackWarning model) ]
            else
                Html.text ""

        listTracks =
            model.pickedTracks
                |> List.indexedMap
                    (\i t ->
                        div [ class "form__question-section form__question-section--table form__question-section--table-auto" ]
                            [ div [ class "form__question-sub-section form__question-sub-section--table" ]
                                [ label [ class "form__label" ]
                                    [ text "Track name *" ]
                                , input
                                    [ class "form__input"
                                    , value t.name
                                    , onInput (UpdatePickedTrack t.id Name)
                                    ]
                                    []
                                ]
                            , div [ class "form__question-sub-section form__question-sub-section--table" ]
                                [ label [ class "form__label" ]
                                    [ text "Track description *" ]
                                , textarea
                                    [ class "form__input form__input--textarea"
                                    , value t.description
                                    , onInput (UpdatePickedTrack t.id Description)
                                    ]
                                    []
                                ]
                            , div [ class "form__question-sub-section form__question-sub-section--table form__question-sub-section--button" ]
                                [ button
                                    [ onClick (DeleteTrack t.id)
                                    , disableInput t.id
                                    , class "button button--secondary icon icon--bin"
                                    ]
                                    []
                                ]
                            ]
                    )

        column1 =
            div []
                [ div []
                    listTracks
                , button
                    [ class "button button--tertiary button--wider"
                    , id "add-new-date-btn"
                    , type_ "button"
                    , onClick AddNewTrack
                    ]
                    [ text "Add New Track" ]
                , validationWarningDiv
                , button
                    [ class "button button--secondary button--wider"
                    , onClick CancelAction
                    ]
                    [ text "Cancel" ]
                , div [ class "bar bar--button" ]
                    [ button [ class "button button--primary button--wider", type_ "button", onClick onClickUpdate ]
                        [ text "Save" ]
                    ]
                ]

        displayWarning =
            if not (String.isEmpty (newTrackWarning model)) then
                "block"
            else
                "none"

        displayDiv =
            if (not model.showNewTrackUi) then
                "none"
            else
                "block"
    in
        div [ class "form form--add-to-view", style [ ( "display", displayDiv ) ] ]
            [ span [ class "form__hint" ]
                [ i [ class "icon icon--warning icon--margin-right" ] [], text "You will be unable to change any tracks that have sessions" ]
            , span [ class "form__hint" ]
                [ span [ class "form__hint form__hint--large" ] [ text "*" ], text " indicates field is mandatory" ]
            , column1
            ]
