module MainUpdate exposing (..)

import Api
import DateUtils
import MainMessages exposing (..)
import MainModel exposing (..)
import String exposing (trim, join, split)
import Ports exposing (..)
import Task
import Date exposing (Date)


addSubmissionIdsInputToSession : String -> Session -> List Submission -> Session
addSubmissionIdsInputToSession submissionIdsInput session submissions =
    let
        validSubmissionIds =
            List.map .id submissions

        submissionIds =
            submissionIdsInput
                |> split ","
                |> List.filterMap (trim >> String.toInt >> Result.toMaybe)
                |> List.filter (\sub -> List.member sub validSubmissionIds)
    in
        { session
            | submissionIds = submissionIds
        }


submissionIdsToInputText : List Int -> String
submissionIdsToInputText submissionIds =
    submissionIds
        |> List.map toString
        |> join ","


getSession model =
    case model.idOfSessionBeingEdited of
        Just id ->
            model.editSession

        Nothing ->
            model.newSession


updateNewColumn : Model -> (Column -> Column) -> Model
updateNewColumn model update =
    ({ model | newColumn = (update model.newColumn) })


updateNewSession : Model -> (Session -> Session) -> Model
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
    updateNewSession model (\ns -> { ns | startTime = update ns.startTime })


updateNewSessionEndTime : Model -> (TimeOfDay -> TimeOfDay) -> Model
updateNewSessionEndTime model update =
    updateNewSession model (\ns -> { ns | endTime = update ns.endTime })


toInt : a -> String -> Int
toInt model string =
    string
        |> String.toInt
        |> Result.withDefault 0


updateModelWithApiUpdateGet : Model -> ApiUpdateGet -> Model
updateModelWithApiUpdateGet model apiUpdateGet =
    ({ model
        | datesWithSessions = apiUpdateGet.datesWithSessions
        , tracks = apiUpdateGet.tracks
        , columns = apiUpdateGet.columns
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
                let
                    firstDate =
                        model.datesWithSessions
                            |> List.map .date
                            |> List.head
                            |> Maybe.withDefault (DateWithoutTime 2017 1 1)
                in
                    ( { model
                        | showNewSessionUi =
                            not model.showNewSessionUi
                                || model.idOfSessionBeingEdited
                                /= Nothing
                        , showNewColumnUi = False
                        , showNewTrackUi = False
                        , showManageDatesUi = False
                        , idOfSessionBeingEdited = Nothing
                        , newSessionDate = firstDate
                        , showPublishUi = False
                        , showPreviewUi = False
                      }
                    , Cmd.none
                    )

            PublishProgrammeBuilder ->
                ( { model
                    | showNewSessionUi = False
                    , showNewColumnUi = False
                    , showNewTrackUi = False
                    , showManageDatesUi = False
                    , idOfSessionBeingEdited = Nothing
                    , showPublishUi = True
                    , showPreviewUi = False
                  }
                , Cmd.none
                )

            TogglePreviewUi ->
                ( { model
                    | showNewSessionUi = False
                    , showNewColumnUi = False
                    , showNewTrackUi = False
                    , showManageDatesUi = False
                    , idOfSessionBeingEdited = Nothing
                    , showPublishUi = False
                    , showPreviewUi = True
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
                    , showPublishUi = False
                    , showPreviewUi = False
                    , pickedTracks = model.tracks
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
                    , showPublishUi = False
                    , showPreviewUi = False
                    , pickedColumns = model.columns
                  }
                , Cmd.none
                )

            ToggleManageDatesUi ->
                let
                    command =
                        if model.datePickerClosed then
                            Ports.openDatepicker ""
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
                        , pickedDates = List.map .date model.datesWithSessions
                        , showPublishUi = False
                        , showPreviewUi = False
                      }
                    , command
                    )

            UpdateColumns ->
                let
                    newColumns =
                        List.sortBy .id model.pickedColumns

                    newColumnToPost =
                        { datesWithSessions = model.datesWithSessions
                        , tracks = model.tracks
                        , columns = newColumns
                        }
                in
                    ( { model
                        | columns = newColumns
                        , newColumn = blankColumn 1
                      }
                    , Api.postModelToDb newColumnToPost model.eventId
                    )

            CreateNewSession ->
                let
                    newSessionWithSubmissionIds =
                        addSubmissionIdsInputToSession model.submissionIdsInput model.newSession model.submissions

                    updateDatesWithSessions =
                        List.map updateDateWithSessions model.datesWithSessions

                    updateDateWithSessions dateWithSessions =
                        { dateWithSessions
                            | sessions =
                                if dateWithSessions.date == model.newSessionDate then
                                    newSessionWithSubmissionIds :: dateWithSessions.sessions
                                else
                                    dateWithSessions.sessions
                        }

                    newSessionToPost =
                        { datesWithSessions = updateDatesWithSessions
                        , tracks = model.tracks
                        , columns = model.columns
                        }
                in
                    ( { model
                        | datesWithSessions = updateDatesWithSessions
                        , newSession = blankSession 1
                        , submissionIdsInput = ""
                      }
                    , Api.postModelToDb newSessionToPost model.eventId
                    )

            UpdateTracks ->
                let
                    newTracks =
                        model.pickedTracks

                    apiUpdatePost =
                        ApiUpdatePost model.datesWithSessions newTracks model.columns
                in
                    ( { model
                        | tracks = newTracks
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
                ( model, Cmd.none )

            SaveModel (Err str) ->
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

            UpdateNewSessionColumn newColumn ->
                if newColumn == "ALL COLUMNS" then
                    -- "ALL COLUMNS" was passed to the update
                    ( (updateNewSession model (\ns -> { ns | sessionColumn = AllColumns })), Cmd.none )
                else
                    case (String.toInt newColumn) of
                        -- a column id integer was passed to the update
                        Ok columnIdInt ->
                            ( (updateNewSession model (\ns -> { ns | sessionColumn = (ColumnId columnIdInt) })), Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

            UpdateNewSessionChair newChair ->
                ( (updateNewSession model (\ns -> { ns | chair = newChair })), Cmd.none )

            UpdateNewSessionLocation newLocation ->
                ( (updateNewSession model (\ns -> { ns | location = newLocation })), Cmd.none )

            UpdateNewSessionTrack newTrackId ->
                ( (updateNewSession model (\ns -> { ns | trackId = (toInt model newTrackId) })), Cmd.none )

            UpdateNewSessionDate newDate ->
                ( { model | newSessionDate = DateUtils.valueStringToDateWithoutTime newDate }, Cmd.none )

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
                    newDatesWithSessions =
                        model.datesWithSessions
                            |> List.map filterDateWithSessions

                    filterDateWithSessions dateWithSessions =
                        { dateWithSessions
                            | sessions = List.filter (\s -> s.id /= sessionId) dateWithSessions.sessions
                        }

                    apiUpdate =
                        { datesWithSessions = newDatesWithSessions
                        , tracks = model.tracks
                        , columns = model.columns
                        }
                in
                    ( { model | datesWithSessions = newDatesWithSessions }
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
                            |> List.concatMap .sessions
                            |> List.filter (\s -> s.id == sessionId)
                            |> List.head
                            |> Maybe.withDefault (blankSession -1)

                    sessionDate =
                        model.datesWithSessions
                            |> List.filter (\s -> List.member session s.sessions)
                            |> List.map .date
                            |> List.head
                            |> Maybe.withDefault (DateWithoutTime 2017 1 1)

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
                        , editSessionDate = sessionDate
                      }
                    , Cmd.none
                    )

            EditSession ->
                case model.idOfSessionBeingEdited of
                    Just id ->
                        let
                            editSessionWithSubmissionIds =
                                addSubmissionIdsInputToSession model.submissionIdsInput model.editSession model.submissions

                            updateDatesWithSessions =
                                List.map updateDateWithSessions model.datesWithSessions

                            updateDateWithSessions dateWithSessions =
                                { dateWithSessions
                                    | sessions =
                                        if dateWithSessions.date == model.editSessionDate then
                                            dateWithSessions
                                                |> .sessions
                                                |> List.filter (\s -> s.id /= id)
                                                |> (::) editSessionWithSubmissionIds
                                        else
                                            dateWithSessions.sessions
                                }

                            editedSession =
                                { editSessionWithSubmissionIds
                                    | id = id
                                }

                            apiUpdate =
                                { datesWithSessions = updateDatesWithSessions
                                , tracks = model.tracks
                                , columns = model.columns
                                }
                        in
                            ( { model
                                | datesWithSessions = updateDatesWithSessions
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
                    allExistingDates =
                        model.datesWithSessions
                            |> List.map .date

                    -- adds new dates to datesWithsessions
                    datesWithSessionsWithNewDates =
                        datesList
                            |> List.map DateUtils.valueStringToDateWithoutTime
                            |> List.filter (\d -> not (List.member d allExistingDates))
                            |> List.map (\d -> { date = d, sessions = [] })
                            |> List.append model.datesWithSessions
                in
                    ( { model | datesWithSessions = datesWithSessionsWithNewDates }, Cmd.none )

            AddNewDate id date ->
                let
                    dateRecord =
                        DateUtils.dateToDateWithoutTime date
                in
                    ( { model
                        | pickedDates =
                            List.append model.pickedDates [ dateRecord ]
                      }
                    , Cmd.batch [ Ports.openDatepicker id ]
                    )

            UpdatePickedDates pickedDatesList ->
                let
                    dateWithoutTimeList =
                        List.map DateUtils.valueStringToDateWithoutTime pickedDatesList
                in
                    ( { model | pickedDates = dateWithoutTimeList }, Cmd.none )

            DeleteDate date ->
                let
                    updatedDatesWithSessions =
                        List.filter (\s -> s.date /= date) model.datesWithSessions

                    updatedPickedDates =
                        List.filter (\d -> d /= date) model.pickedDates
                in
                    ( { model
                        | datesWithSessions = updatedDatesWithSessions
                        , pickedDates = updatedPickedDates
                      }
                    , Cmd.none
                    )

            GetDateAndThenAddDate id ->
                model ! [ Task.perform (AddNewDate id) Date.now ]

            DeleteTrack trackId ->
                let
                    newTracks =
                        List.filter (\t -> t.id /= trackId) model.tracks

                    apiUpdate =
                        { datesWithSessions = model.datesWithSessions
                        , tracks = newTracks
                        , columns = model.columns
                        }
                in
                    ( { model | tracks = newTracks }
                    , Api.postModelToDb apiUpdate model.eventId
                    )

            AddNewTrack ->
                ( { model
                    | pickedTracks =
                        appendNewElementToList model.pickedTracks (Track 0 "" "")
                  }
                , Cmd.none
                )

            UpdatePickedTrack trackId trackInputType newPickedTrackInput ->
                let
                    pickedTrack =
                        model.pickedTracks
                            |> List.filter (\t -> t.id == trackId)
                            |> List.head
                            |> Maybe.withDefault (Track 0 "" "")

                    newPickedTrack =
                        if trackInputType == "name" then
                            { pickedTrack
                                | name = newPickedTrackInput
                            }
                        else if trackInputType == "description" then
                            { pickedTrack
                                | description = newPickedTrackInput
                            }
                        else
                            pickedTrack

                    pickedTracksWithoutUpdatedTrack =
                        model.pickedTracks
                            |> List.filter (\s -> s.id /= trackId)
                in
                    ( { model | pickedTracks = List.append pickedTracksWithoutUpdatedTrack [ newPickedTrack ] }, Cmd.none )

            -- this deletes columns from pickedcolumns list - deletion isn't saved the user clicks 'save changes'
            DeleteColumn columnId ->
                let
                    newPickedColumns =
                        List.filter (\c -> c.id /= columnId) model.pickedColumns
                in
                    ( { model | pickedColumns = newPickedColumns }
                    , Cmd.none
                    )

            AddNewColumn ->
                ( { model
                    | pickedColumns =
                        appendNewElementToList model.pickedColumns (Column 0 "")
                  }
                , Cmd.none
                )

            UpdatePickedColumn columnId newPickedColumnInput ->
                let
                    pickedColumn =
                        model.pickedColumns
                            |> List.filter (\c -> c.id == columnId)
                            |> List.head
                            |> Maybe.withDefault (Column 0 "")

                    newPickedColumn =
                        { pickedColumn
                            | name = newPickedColumnInput
                        }

                    pickedColumnsWithoutUpdatedColumn =
                        model.pickedColumns
                            |> List.filter (\c -> c.id /= columnId)
                in
                    ( { model
                        | pickedColumns = newPickedColumn :: pickedColumnsWithoutUpdatedColumn
                      }
                    , Cmd.none
                    )
