module MobileView exposing (..)

import Date
import DateUtils
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on)
import MainMessages exposing (..)
import MainModel exposing (..)
import Time
import Utils
import Json.Decode


view : Model -> Html Msg
view model =
    -- TODO: SORT BY TIME. abstract some of the functions
    -- - TODO : PROGRAMME TABS AT THE TOP
    let
        numColumns =
            List.length model.displayedColumns

        displayedColumnId =
            model.displayedColumns
                |> List.map .id
                |> List.head
                |> Maybe.withDefault 1

        columnOptions =
            (List.map (\c -> option [ value (toString c.id) ] [ text c.name ]) model.columns)

        dateDivs =
            (List.map
                (\d ->
                    div []
                        [ span []
                            [ text ((DateUtils.dateWithoutTimeToDay d.date) ++ " " ++ toString d.date.day ++ " " ++ (DateUtils.intToMonthString d.date.month))
                            ]
                        , div []
                            (List.map
                                (\s ->
                                    let
                                        locationId =
                                            case s.locationId of
                                                Just locationId ->
                                                    locationId

                                                Nothing ->
                                                    0

                                        trackId =
                                            case s.trackId of
                                                Just trackId ->
                                                    trackId

                                                Nothing ->
                                                    0

                                        chairId =
                                            case s.chairId of
                                                Just chairId ->
                                                    chairId

                                                Nothing ->
                                                    0
                                    in
                                        if s.sessionColumn == ColumnId displayedColumnId then
                                            div []
                                                [ text s.name
                                                , span [] [ text (getNameFromId model.locations locationId) ]
                                                , span [] [ text (getNameFromId model.tracks trackId) ]
                                                , span [] [ text (getNameFromId model.chairs chairId) ]
                                                , span []
                                                    [ text
                                                        (toString s.startTime.hour
                                                            ++ ":"
                                                            ++ DateUtils.add0Padding (toString s.startTime.minute)
                                                            ++ "-"
                                                            ++ toString s.endTime.hour
                                                            ++ ":"
                                                            ++ DateUtils.add0Padding (toString s.endTime.minute)
                                                        )
                                                    ]
                                                ]
                                        else
                                            div [] []
                                )
                                d.sessions
                            )
                        ]
                )
                model.datesWithSessions
            )
    in
        div []
            [ div []
                [ select [ onChange UpdateDisplayedColumn ]
                    columnOptions
                ]
            , div [] dateDivs
            ]


onChange : (String -> msg) -> Attribute msg
onChange handler =
    on "change" <| Json.Decode.map handler <| Json.Decode.at [ "target", "value" ] Json.Decode.string


getNameFromId list id =
    list
        |> List.filter (\t -> t.id == id)
        |> List.map .name
        |> List.head
        |> Maybe.withDefault ""
