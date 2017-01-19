module MainUpdate exposing (..)

import DateUtils
import MainMessages exposing (..)
import MainModel exposing (..)


updateNewColumn : { b | newColumn : a } -> (a -> c) -> { b | newColumn : c }
updateNewColumn model update =
    ({ model | newColumn = (update model.newColumn) })


updateNewSession : { b | newSession : a } -> (a -> c) -> { b | newSession : c }
updateNewSession model update =
    ({ model | newSession = (update model.newSession) })


updateNewTrack : { b | newTrack : a } -> (a -> c) -> { b | newTrack : c }
updateNewTrack model update =
    ({ model | newTrack = (update model.newTrack) })


updateNewSessionStartTime :
    { b | newSession : { c | startTime : a } }
    -> (a -> d)
    -> { b | newSession : { c | startTime : d } }
updateNewSessionStartTime model update =
    updateNewSession model (\ns -> { ns | startTime = update ns.startTime })


updateNewSessionEndTime :
    { b | newSession : { c | endTime : a } }
    -> (a -> d)
    -> { b | newSession : { c | endTime : d } }
updateNewSessionEndTime model update =
    updateNewSession model (\ns -> { ns | endTime = update ns.endTime })


toInt : a -> String -> Int
toInt model string =
    string
        |> String.toInt
        |> Result.withDefault 0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTrack ->
            ( model, Cmd.none )

        NewColumn ->
            ( model, Cmd.none )

        ToggleNewSessionUi ->
            ( { model
                | showNewSessionUi = not model.showNewSessionUi
                , showNewColumnUi = False
                , showNewTrackUi = False
                , idOfSessionBeingEdited = Nothing
              }
            , Cmd.none
            )

        ToggleNewTrackUi ->
            ( { model
                | showNewTrackUi = not model.showNewTrackUi
                , showNewColumnUi = False
                , showNewSessionUi = False
              }
            , Cmd.none
            )

        ToggleNewColumnUi ->
            ( { model
                | showNewColumnUi = not model.showNewColumnUi
                , showNewSessionUi = False
                , showNewTrackUi = False
              }
            , Cmd.none
            )

        CreateNewColumn ->
            let
                highestColumnId =
                    model.columns
                        |> List.map .id
                        |> List.maximum
                        |> Maybe.withDefault 0

                newColumn =
                    model.newColumn

                newColumnWithId =
                    { newColumn | id = highestColumnId + 1 }
            in
                ( { model
                    | columns = model.columns ++ [ newColumnWithId ]
                    , newColumn = blankColumn 1
                  }
                , Cmd.none
                )

        CreateNewSession ->
            let
                newSessionId =
                    model.sessions
                        |> List.map .id
                        |> List.maximum
                        |> Maybe.withDefault 1

                newSession =
                    model.newSession

                newSessionWithId =
                    { newSession | id = newSessionId }
            in
                ( { model
                    | sessions = model.sessions ++ [ newSessionWithId ]
                    , newSession = blankSession 1
                  }
                , Cmd.none
                )

        CreateNewTrack ->
            let
                highestTrackId =
                    model.tracks
                        |> List.map .id
                        |> List.maximum
                        |> Maybe.withDefault 0

                newTrack =
                    model.newTrack

                newTrackWithId =
                    { newTrack | id = highestTrackId + 1 }
            in
                ( { model
                    | tracks = model.tracks ++ [ newTrackWithId ]
                    , newTrack = blankTrack 1
                  }
                , Cmd.none
                )

        UpdateNewColumnName newName ->
            ( (updateNewColumn model (\ns -> { ns | name = newName })), Cmd.none )

        UpdateNewSessionName newName ->
            ( (updateNewSession model (\ns -> { ns | name = newName })), Cmd.none )

        UpdateNewTrackName newName ->
            ( (updateNewTrack model (\ns -> { ns | name = newName })), Cmd.none )

        UpdateNewSessionDescription newDescription ->
            ( (updateNewSession model (\ns -> { ns | description = newDescription })), Cmd.none )

        UpdateNewSessionColumn newColumnId ->
            ( (updateNewSession model (\ns -> { ns | columnId = (toInt model newColumnId) })), Cmd.none )

        UpdateNewSessionChair newChair ->
            ( (updateNewSession model (\ns -> { ns | chair = newChair })), Cmd.none )

        UpdateNewSessionLocation newLocation ->
            ( (updateNewSession model (\ns -> { ns | location = newLocation })), Cmd.none )

        UpdateNewSessionTrack newTrackId ->
            ( (updateNewSession model (\ns -> { ns | trackId = (toInt model newTrackId) })), Cmd.none )

        UpdateNewSessionDate newDate ->
            ( (updateNewSession model (\ns -> { ns | date = (DateUtils.valueStringToDateWithoutTime newDate) })), Cmd.none )

        UpdateNewSessionStartHour new ->
            ( updateNewSessionStartTime model (\st -> { st | hour = clamp 0 23 (toInt model new) }), Cmd.none )

        UpdateNewSessionStartMinute new ->
            ( updateNewSessionStartTime model (\st -> { st | minute = clamp 0 59 (toInt model new) }), Cmd.none )

        UpdateNewSessionEndHour new ->
            ( updateNewSessionEndTime model (\et -> { et | hour = clamp 0 23 (toInt model new) }), Cmd.none )

        UpdateNewSessionEndMinute new ->
            ( updateNewSessionEndTime model (\et -> { et | minute = clamp 0 59 (toInt model new) }), Cmd.none )

        DeleteSession sessionId ->
            ( { model | sessions = List.filter (\s -> s.id /= sessionId) model.sessions }, Cmd.none )

        SelectSessionToEdit sessionId ->
            let
                isAlreadySelected =
                    model.idOfSessionBeingEdited
                        |> Maybe.withDefault -1
                        |> (\id -> id == sessionId)

                session =
                    model.sessions
                        |> List.filter (\s -> s.id == sessionId)
                        |> List.head
                        |> Maybe.withDefault (blankSession -1)
            in
                ( { model
                    | idOfSessionBeingEdited =
                        if isAlreadySelected then
                            Nothing
                        else
                            Just sessionId
                    , showNewSessionUi = False
                    , newSession = session
                  }
                , Cmd.none
                )
