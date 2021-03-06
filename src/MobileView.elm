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
    let
        displayedColumnId =
            Maybe.withDefault 0 model.displayedColumn

        columnOptions =
            (List.map (\c -> option [ value (toString c.id) ] [ text c.name ]) model.columns)

        dateDivs =
            (List.map
                (\d ->
                    let
                        sortedSessions =
                            d.sessions
                                |> List.sortBy (\s -> DateUtils.timeOfDayToTime d.date s.startTime)
                    in
                        div [ class "prog-mob__oneday" ]
                            [ div [ class "prog-mob__datecontainer" ]
                                [ span [ class "prog-mob__day" ]
                                    [ text (DateUtils.dateWithoutTimeToDay d.date)
                                    ]
                                , span [ class "prog-mob__date" ] [ text (toString d.date.day ++ " " ++ (DateUtils.intToMonthString d.date.month)) ]
                                ]
                            , div []
                                (List.map
                                    (\s ->
                                        let
                                            locationId =
                                                Maybe.withDefault 0 s.locationId

                                            trackId =
                                                Maybe.withDefault 0 s.trackId

                                            chairId =
                                                Maybe.withDefault 0 s.chairId

                                            query =
                                                if model.showPreviewUi then
                                                    "?view=preview"
                                                else if model.showPublishPage then
                                                    "?view=published"
                                                else if model.showBasicPage then
                                                    "?view=basic"
                                                else
                                                    ""
                                        in
                                            if s.sessionColumn == ColumnId displayedColumnId || s.sessionColumn == AllColumns then
                                                div [ class "prog-mob__session" ]
                                                    [ div
                                                        [ class "prog-mob__sesh-times" ]
                                                        [ span [ class "prog-mob__sesh-time prog-mob__sesh-time--start" ]
                                                            [ text
                                                                (toString s.startTime.hour
                                                                    ++ "."
                                                                    ++ DateUtils.add0Padding (toString s.startTime.minute)
                                                                )
                                                            ]
                                                        , span [ class "prog-mob__sesh-time prog-mob__sesh-time--end" ]
                                                            [ text
                                                                (toString s.endTime.hour
                                                                    ++ "."
                                                                    ++ DateUtils.add0Padding (toString s.endTime.minute)
                                                                )
                                                            ]
                                                        ]
                                                    , div [ class "prog-mob__sesh-info" ]
                                                        [ a
                                                            [ class "prog-mob__sesh-name"
                                                            , href
                                                                ("/events/"
                                                                    ++ model.eventId
                                                                    ++ "/sessions/"
                                                                    ++ (toString s.id)
                                                                    ++ query
                                                                )
                                                            ]
                                                            [ text s.name ]
                                                        , span [ class "prog-mob__sesh-chair" ] [ text (getNameFromId model.chairs chairId) ]
                                                        , span [ class "prog-mob__sesh-location" ] [ text (getNameFromId model.locations locationId) ]
                                                        , span [ class "prog-mob__sesh-track" ] [ text (getNameFromId model.tracks trackId) ]
                                                        ]
                                                    ]
                                            else
                                                div [] []
                                    )
                                    sortedSessions
                                )
                            ]
                )
                model.datesWithSessions
            )
    in
        div [ class "prog-mob__container" ]
            [ div [ class "prog-mob__colselect" ]
                [ select [ onChange UpdateDisplayedColumn ]
                    columnOptions
                ]
            , div [ class "prog-mob__feed" ] dateDivs
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
