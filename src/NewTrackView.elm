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
                                [ class "form-control"
                                , value t.name
                                , onInput (UpdatePickedTrack t.id "name")
                                ]
                                []
                            , input
                                [ class "form-control"
                                , value t.description
                                , onInput (UpdatePickedTrack t.id "description")
                                ]
                                []
                            , button
                                [ onClick (DeleteTrack t.id)
                                , style [ ( "margin-left", "0.2rem" ) ]
                                , disableInput t.id
                                ]
                                [ text "Delete" ]
                            ]
                    )

        column1 =
            div [ class "form-group" ]
                [ div [ class "input-group" ]
                    listTracks
                , div [ style [ ( "margin-top", "1rem" ) ] ]
                    [ button
                        [ class "btn btn-default"
                        , id "add-new-date-btn"
                        , type_ "button"
                        , onClick AddNewTrack
                        ]
                        [ text "Add New Track" ]
                    ]
                , div [ style [ ( "margin-top", "1rem" ) ] ] [ text (newTrackWarning model) ]
                , div [ style [ ( "margin-top", "1rem" ) ] ]
                    [ button [ class "btn btn-default", type_ "button", disabled (newTrackWarning model /= ""), onClick UpdateTracks ]
                        [ text "Save Tracks" ]
                    ]
                ]
    in
        div [ hidden (not model.showNewTrackUi), class "row" ]
            [ div [ class "col-md-4" ] [ column1 ]
            ]
