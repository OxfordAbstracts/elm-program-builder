module MainUpdate exposing (update)

import MainMessages exposing (..)
import MainModel exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateNewSession update =
            ({ model | newSession = (update model.newSession) })

        toInt string =
            string
                |> String.toInt
                |> Result.withDefault 1
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

            UpdateNewSessionName newName ->
                ( (updateNewSession (\ns -> { ns | name = newName })), Cmd.none )

            UpdateNewSessionDescription newDescription ->
                ( (updateNewSession (\ns -> { ns | description = newDescription })), Cmd.none )

            -- UpdateNewStartTime newStartTime ->
            --     ( (updateNewSession (\ns -> { ns | startTime = newStartTime })), Cmd.none )
            --
            -- UpdateNewEndTime newName ->
            --     ( (updateNewSession (\ns -> { ns | endTime = newName })), Cmd.none )
            UpdateNewSessionColumn newColumnId ->
                ( (updateNewSession (\ns -> { ns | columnId = (toInt newColumnId) })), Cmd.none )

            UpdateNewSessionTrack newTrackId ->
                ( (updateNewSession (\ns -> { ns | trackId = (toInt newTrackId) })), Cmd.none )

            UpdateNewSessionDay newDay ->
                ( model, Cmd.none )
