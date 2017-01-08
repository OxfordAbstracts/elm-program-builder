module MainUpdate exposing (update)

import DateUtils
import MainMessages exposing (..)
import MainModel exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateNewSession update =
            ({ model | newSession = (update model.newSession) })

        updateNewSessionStartTime update =
            updateNewSession (\ns -> { ns | startTime = update ns.startTime })

        updateNewSessionEndTime update =
            updateNewSession (\ns -> { ns | endTime = update ns.endTime })

        toInt string =
            string
                |> String.toInt
                |> Result.withDefault 0
    in
        case msg of
            NewSession ->
                ( model, Cmd.none )

            NewTrack ->
                ( model, Cmd.none )

            NewColumn ->
                ( model, Cmd.none )

            ToggleNewSessionUi ->
                ( { model | showNewSessionUi = not model.showNewSessionUi }, Cmd.none )

            ToggleNewTrackUi ->
                ( model, Cmd.none )

            ToggleNewColumnUi ->
                ( model, Cmd.none )

            UpdateNewSessionBlurred ->
                ( { model | newSessionBlurred = model.newSession }, Cmd.none )

            UpdateNewSessionName newName ->
                ( (updateNewSession (\ns -> { ns | name = newName })), Cmd.none )

            UpdateNewSessionDescription newDescription ->
                ( (updateNewSession (\ns -> { ns | description = newDescription })), Cmd.none )

            UpdateNewSessionColumn newColumnId ->
                ( (updateNewSession (\ns -> { ns | columnId = (toInt newColumnId) })), Cmd.none )

            UpdateNewSessionTrack newTrackId ->
                ( (updateNewSession (\ns -> { ns | trackId = (toInt newTrackId) })), Cmd.none )

            UpdateNewSessionDate newDate ->
                ( (updateNewSession (\ns -> { ns | date = (DateUtils.valueStringToDateWithoutTime newDate) })), Cmd.none )

            UpdateNewSessionStartHour new ->
                ( updateNewSessionStartTime (\st -> { st | hour = clamp 0 23 (toInt new) }), Cmd.none )

            UpdateNewSessionStartMinute new ->
                ( updateNewSessionStartTime (\st -> { st | minute = clamp 0 59 (toInt new) }), Cmd.none )

            UpdateNewSessionEndHour new ->
                ( updateNewSessionEndTime (\et -> { et | hour = clamp 0 23 (toInt new) }), Cmd.none )

            UpdateNewSessionEndMinute new ->
                ( updateNewSessionEndTime (\et -> { et | minute = clamp 0 59 (toInt new) }), Cmd.none )
