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

        listTracks =
            model.pickedTracks
                |> List.sortBy .id
                |> List.indexedMap
                    (\i t ->
                        div [ class "form__question-section form__question-section--table" ]
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
                            , div [ class "form__question-sub-section form__question-sub-section--table form__question-sub-section__button" ]
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
                    [ class "button button--tertiary"
                    , id "add-new-date-btn"
                    , type_ "button"
                    , onClick AddNewTrack
                    ]
                    [ text "Add New Track" ]
                , span [ class "prog-form--warning" ] [ text (newTrackWarning model) ]
                , div []
                    [ button [ class "button button--primary", type_ "button", disabled (newTrackWarning model /= ""), onClick UpdateTracks ]
                        [ text "Save" ]
                    ]
                ]

        displayDiv =
            if (not model.showNewTrackUi) then
                "none"
            else
                "block"
    in
        div [ class "form form--add-to-view", style [ ( "display", displayDiv ) ] ]
            [ span [ class "form__hint" ]
                [ span [ class "form__hint form__hint--large" ] [ text "*" ], text " indicates field is mandatory" ]
            , div []
                [ column1 ]
            ]
