module MainUpdate exposing (..)

import Api
import DateUtils
import MainMessages exposing (..)
import MainModel exposing (..)
import String exposing (trim, join, split)
import Ports exposing (..)


addSubmissionIdsInputToSession : String -> Session -> List Submission -> Session
addSubmissionIdsInputToSession submissionIdsInput session submissions =
    let
        submissionIds =
            submissionIdsInput
                |> split ","
                |> List.filterMap (trim >> String.toInt >> Result.toMaybe)

        validSubmissionIds =
            List.map .id submissions

        -- removes any invalid submissions - i.e. ones that have not been accepted for this event
        submissionIdsValid =
            submissionIds
                |> List.filter (\sub -> List.member sub validSubmissionIds)
    in
        { session
            | submissionIds = submissionIdsValid
        }


submissionIdsToInputText : List Int -> String
submissionIdsToInputText submissionIds =
    submissionIds
        |> List.map toString
        |> join ","


updateNewColumn : Model -> (Column -> Column) -> Model
updateNewColumn model update =
    ({ model | newColumn = (update model.newColumn) })


updateNewSession : Model -> (DateWithNewSession -> DateWithNewSession) -> Model
updateNewSession model update =
    case model.idOfSessionBeingEdited of
        Just id ->
            ({ model | editSession = (update model.editSession) })

        Nothing ->
            ({ model | newSession = (update model.newSession) })


updateNewTrack : Model -> (Track -> Track) -> Model
updateNewTrack model update =
    ({ model | newTrack = (update model.newTrack) })


updateNewSessionStartTime : Model -> (TimeOfDay -> TimeOfDay) -> Model
updateNewSessionStartTime model update =
    updateNewSession model (\ns -> { ns | startTime = update ns.session.startTime })


updateNewSessionEndTime : Model -> (TimeOfDay -> TimeOfDay) -> Model
updateNewSessionEndTime model update =
    updateNewSession model (\ns -> { ns | endTime = update ns.session.endTime })


toInt : a -> String -> Int
toInt model string =
    string
        |> String.toInt
        |> Result.withDefault 0


updateModelWithApiUpdateGet : Model -> ApiUpdateGet -> Model
updateModelWithApiUpdateGet model apiUpdateGet =
    ({ model
        | datesWithSessions =
            apiUpdateGet.datesWithSessions
            -- sessions = apiUpdateGet.sessions
        , tracks = apiUpdateGet.tracks
        , columns =
            apiUpdateGet.columns
            -- , dates = apiUpdateGet.dates
        , submissions = apiUpdateGet.submissions
     }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        appendNewElementToList list newElement =
            let
                highestId =
                    list
                        |> List.map .id
                        |> List.maximum
                        |> Maybe.withDefault 0

                newElementWithId =
                    { newElement | id = highestId + 1 }
            in
                list ++ [ newElementWithId ]
    in
        case msg of
            NewTrack ->
                ( model, Cmd.none )

            NewColumn ->
                ( model, Cmd.none )

            ToggleNewSessionUi ->
                ( { model
                    | showNewSessionUi =
                        not model.showNewSessionUi
                            || model.idOfSessionBeingEdited
                            /= Nothing
                    , showNewColumnUi = False
                    , showNewTrackUi = False
                    , showManageDatesUi = False
                    , idOfSessionBeingEdited = Nothing
                  }
                , Cmd.none
                )

            ToggleNewTrackUi ->
                ( { model
                    | showNewTrackUi = not model.showNewTrackUi
                    , showNewColumnUi = False
                    , showNewSessionUi = False
                    , showManageDatesUi = False
                    , idOfSessionBeingEdited = Nothing
                  }
                , Cmd.none
                )

            ToggleNewColumnUi ->
                ( { model
                    | showNewColumnUi = not model.showNewColumnUi
                    , showNewSessionUi = False
                    , showNewTrackUi = False
                    , showManageDatesUi = False
                    , idOfSessionBeingEdited = Nothing
                  }
                , Cmd.none
                )

            ToggleManageDatesUi ->
                let
                    command =
                        if (model.datePickerClosed) then
                            Ports.openDatepicker ("")
                        else
                            Cmd.none
                in
                    ( { model
                        | showManageDatesUi = not model.showManageDatesUi
                        , showNewSessionUi = False
                        , showNewTrackUi = False
                        , showNewColumnUi = False
                        , idOfSessionBeingEdited = Nothing
                        , datePickerClosed = False
                        , pickedDates = model.dates
                      }
                    , command
                    )

            CreateNewColumn ->
                let
                    listWithNewId =
                        appendNewElementToList model.columns model.newColumn

                    newColumnToPost =
                        { datesWithSessions = model.datesWithSessions
                        , tracks = model.tracks
                        , columns =
                            listWithNewId
                            -- , dates = model.dates
                        }
                in
                    ( { model
                        | columns = listWithNewId
                        , newColumn = blankColumn 1
                      }
                    , Api.postModelToDb newColumnToPost model.eventId
                    )

            CreateNewSession ->
                let
                    y =
                        Debug.log "model.newSession" model.newSession

                    newSessionWithSubmissionIds =
                        addSubmissionIdsInputToSession model.submissionIdsInput model.newSession model.submissions

                    x =
                        Debug.log "newSessionWithSubmissionIds" newSessionWithSubmissionIds

                    listWithNewId =
                        -- appendNewElementToList model.datesWithSessions newSessionWithSubmissionIds
                        model.datesWithSessions
                            |> List.filter (\d -> d.date == model.newSession.date)
                            |> (::) model.newSession

                    -- appendNewElementToList list newElement =
                    --     let
                    --         highestId =
                    --             list
                    --                 |> List.map .id
                    --                 |> List.maximum
                    --                 |> Maybe.withDefault 0
                    --
                    --         newElementWithId =
                    --             { newElement | id = highestId + 1 }
                    --     in
                    --         list ++ [ newElementWithId ]
                    newSessionToPost =
                        { datesWithSessions = listWithNewId
                        , tracks = model.tracks
                        , columns =
                            model.columns
                            -- , dates = model.dates
                        }
                in
                    ( { model
                        | datesWithSessions = listWithNewId
                        , newSession = blankSession 1
                        , submissionIdsInput = ""
                      }
                    , Api.postModelToDb newSessionToPost model.eventId
                    )

            CreateNewTrack ->
                let
                    tracksWithNewId =
                        appendNewElementToList model.tracks model.newTrack

                    apiUpdatePost =
                        ApiUpdatePost model.datesWithSessions tracksWithNewId model.columns
                in
                    ( { model
                        | tracks = tracksWithNewId
                        , newTrack = blankTrack 1
                      }
                    , Api.postModelToDb apiUpdatePost model.eventId
                    )

            UpdateModel (Ok apiUpdateGet) ->
                let
                    updatedModel =
                        updateModelWithApiUpdateGet model apiUpdateGet
                in
                    ( updatedModel, Cmd.none )

            UpdateModel (Err str) ->
                Debug.log (toString str)
                    ( model, Cmd.none )

            SaveModel (Err str) ->
                Debug.log (toString str)
                    ( model, Cmd.none )

            SaveModel (Ok apiUpdate) ->
                ( model, Cmd.none )

            UpdateNewColumnName newName ->
                ( (updateNewColumn model (\nc -> { nc | name = newName })), Cmd.none )

            UpdateNewSessionName newName ->
                ( (updateNewSession model (\ns -> { ns | name = newName })), Cmd.none )

            UpdateNewTrackName newName ->
                ( (updateNewTrack model (\nt -> { nt | name = newName })), Cmd.none )

            UpdateNewTrackDescription newDescription ->
                ( (updateNewTrack model (\nt -> { nt | description = newDescription })), Cmd.none )

            UpdateNewSessionDescription newDescription ->
                ( (updateNewSession model (\ns -> { ns | description = newDescription })), Cmd.none )

            UpdateNewSessionSubmissionIds newSubmissionIdsString ->
                ( { model | submissionIdsInput = newSubmissionIdsString }, Cmd.none )

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
                let
                    newSessionsList =
                        model.datesWithSessions
                            |> List.filter (\s -> s.id /= sessionId)

                    apiUpdate =
                        { datesWithSessions = newSessionsList
                        , tracks = model.tracks
                        , columns =
                            model.columns
                            -- , dates = model.dates
                        }
                in
                    ( { model | datesWithSessions = newSessionsList }
                    , Api.postModelToDb apiUpdate model.eventId
                    )

            SelectSessionToEdit sessionId ->
                let
                    isAlreadySelected =
                        model.idOfSessionBeingEdited
                            |> Maybe.withDefault -1
                            |> (\id -> id == sessionId)

                    session =
                        model.datesWithSessions
                            |> List.filter (\s -> s.id == sessionId)
                            |> List.head
                            |> Maybe.withDefault (blankSession -1)

                    submissionIdsInput =
                        submissionIdsToInputText session.submissionIds
                in
                    ( { model
                        | idOfSessionBeingEdited =
                            if isAlreadySelected then
                                Nothing
                            else
                                Just sessionId
                        , showNewSessionUi =
                            if isAlreadySelected then
                                False
                            else
                                True
                        , showNewTrackUi = False
                        , showNewColumnUi = False
                        , editSession = session
                        , submissionIdsInput = submissionIdsInput
                      }
                    , Cmd.none
                    )

            EditSession ->
                case model.idOfSessionBeingEdited of
                    Just id ->
                        let
                            listWithoutSessionBeingEdited =
                                model.datesWithSessions
                                    |> List.filter (\s -> s.id /= id)

                            editSessionWithSubmissionIds =
                                addSubmissionIdsInputToSession model.submissionIdsInput model.editSession model.submissions

                            editedSession =
                                { editSessionWithSubmissionIds
                                    | id = id
                                }

                            apiUpdate =
                                { datesWithSessions = listWithoutSessionBeingEdited ++ [ editedSession ]
                                , tracks = model.tracks
                                , columns =
                                    model.columns
                                    -- , dates = model.dates
                                }
                        in
                            ( { model
                                | datesWithSessions = listWithoutSessionBeingEdited ++ [ editedSession ]
                                , editSession = blankSession 1
                                , showNewSessionUi = False
                                , idOfSessionBeingEdited = Nothing
                                , submissionIdsInput = ""
                              }
                            , Api.postModelToDb apiUpdate model.eventId
                            )

                    Nothing ->
                        ( model, Cmd.none )

            UpdateDates datesList ->
                let
                    dateWithoutTimeList =
                        datesList
                            |> List.map DateUtils.valueStringToDateWithoutTime

                    -- dates =
                    --     List.append model.dates dateWithoutTimeList
                in
                    ( { model | dates = dateWithoutTimeList }, Cmd.none )

            -- GetTodayAndAdd id ->
            --     ( model, Date.now (AddNewDate id) )
            AddNewDate id ->
                -- change to getTodayAndAdd
                ( { model
                    | pickedDates =
                        List.append [ { year = 2017, month = 2, day = 14 } ] model.pickedDates
                  }
                , Cmd.batch [ Ports.openDatepicker (id) ]
                )

            UpdatePickedDates pickedDatesList ->
                let
                    dateWithoutTimeList =
                        pickedDatesList
                            |> List.map DateUtils.valueStringToDateWithoutTime
                in
                    ( { model | pickedDates = dateWithoutTimeList }, Cmd.none )
