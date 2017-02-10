module NewTrackView exposing (view, newTrackWarning)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import MainMessages exposing (..)
import MainModel exposing (..)
import MainMessages exposing (..)
import GetWarning exposing (..)


newTrackWarning model =
    if model.showNewTrackUi && model.newTrack.name == "" then
        getWarning "Track name field is empty" model
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

        column1 =
            div [ class "form-group" ]
                [ div [ class "input-group" ]
                    [ label [ for "track-name-input" ]
                        [ text "Track name" ]
                    , input
                        [ class "form-control"
                        , id "track-name-input"
                        , type_ "text"
                        , value model.newTrack.name
                        , onInput UpdateNewTrackName
                        ]
                        []
                    ]
                , div [ class "input-group" ]
                    [ label [ for "track-description-input" ]
                        [ text "Track description" ]
                    , input
                        [ class "form-control"
                        , id "track-description-input"
                        , type_ "text"
                        , value model.newTrack.description
                        , onInput UpdateNewTrackDescription
                        ]
                        []
                    ]
                , div [ style [ ( "margin-top", "1rem" ) ] ] [ text (newTrackWarning model) ]
                , div [ style [ ( "margin-top", "1rem" ) ] ]
                    [ button [ class "btn btn-default", type_ "button", disabled (newTrackWarning model /= ""), onClick CreateNewTrack ]
                        [ text "Create Track" ]
                    ]
                ]
    in
        div [ hidden (not model.showNewTrackUi), class "row" ]
            [ div [ class "col-md-4" ] [ column1 ]
            ]
