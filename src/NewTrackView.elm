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
                if List.member trackId tracksWithSessions then
                    disabled True
                else
                    disabled False

        listTracks =
            model.pickedTracks
                |> List.sortBy .id
                |> List.indexedMap
                    (\i t ->
                        div []
                            [ input
                                [ class "form__input"
                                , value t.name
                                , onInput (UpdatePickedTrack t.id Name)
                                ]
                                []
                            , input
                                [ class "form__input"
                                , value t.description
                                , onInput (UpdatePickedTrack t.id Description)
                                ]
                                []
                            , button
                                [ onClick (DeleteTrack t.id)
                                , style [ ( "margin-left", "0.2rem" ) ]
                                , disableInput t.id
                                , class "button button--secondary icon icon--bin"
                                ]
                                []
                            ]
                    )

        column1 =
            div [ class "form" ]
                [ div [ class "form__question-sub-section--inline" ]
                    listTracks
                , div [ style [ ( "margin-top", "1rem" ) ] ]
                    [ button
                        [ class "button button--tertiary"
                        , id "add-new-date-btn"
                        , type_ "button"
                        , onClick AddNewTrack
                        ]
                        [ text "Add New Track" ]
                    ]
                , div [ style [ ( "margin-top", "1rem" ) ] ] [ text (newTrackWarning model) ]
                , div [ style [ ( "margin-top", "1rem" ) ] ]
                    [ button [ class "button button--primary button--cta", type_ "button", disabled (newTrackWarning model /= ""), onClick UpdateTracks ]
                        [ text "Save Changes" ]
                    ]
                ]
    in
        div [ hidden (not model.showNewTrackUi), class "row" ]
            [ div [ class "col-md-4" ] [ column1 ]
            ]
