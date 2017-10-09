module MainUpdate exposing (..)

import Api
import DateUtils
import MainMessages exposing (..)
import MainModel exposing (..)
import String exposing (trim, join, split)
import Ports exposing (..)
import Task
import Set
import Date exposing (Date)
import List.Extra


addSubmissionIdsInputToSession : List SubmissionIdInput -> Session -> List Submission -> Session
addSubmissionIdsInputToSession submissionIdInputs session submissions =
    let
        validSubmissionIds =
            List.map .id submissions

        getSessionSubmissionFromInputs submissionIdInputs =
            submissionIdInputs
                |> List.concatMap getSessionSubmissionFromInput
                |> List.Extra.uniqueBy .id

        getSessionSubmissionFromInput submissionIdInput =
            submissionIdInput
                |> .submissionIds
                |> split ","
                |> List.filterMap (trim >> String.toInt >> Result.toMaybe)
                |> List.filter (\sub -> List.member sub validSubmissionIds)
                |> List.map (addTimes submissionIdInput.startTime submissionIdInput.endTime)

        addTimes startTime endTime id =
            { id = id
            , startTime = startTime
            , endTime = endTime
            }
    in
        { session
            | submissions = getSessionSubmissionFromInputs submissionIdInputs
        }


submissionsToInputText : List SessionSubmission -> List SubmissionIdInput
submissionsToInputText sessionSubmissions =
    let
        getSubmissionIdInputs sub idInputs =
            case List.Extra.find (sameStartAndEndTimes sub) idInputs of
                Nothing ->
                    idInputs
                        ++ [ { submissionIds = toString sub.id
                             , startTime = sub.startTime
                             , endTime = sub.endTime
                             , id = generateId idInputs
                             }
                           ]

                Just _ ->
                    List.Extra.updateIf (sameStartAndEndTimes sub) (update sub) idInputs

        sameStartAndEndTimes x y =
            x.startTime == y.startTime && x.endTime == y.endTime

        update sub idInput =
            { idInput | submissionIds = idInput.submissionIds ++ ", " ++ (toString sub.id) }
    in
        List.foldl getSubmissionIdInputs [] sessionSubmissions


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
        , locations = apiUpdateGet.locations
        , chairs = apiUpdateGet.chairs
        , submissions = apiUpdateGet.submissions
        , published = apiUpdateGet.published
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
                        , showManageLocationsUi = False
                        , showManageChairsUi = False
                        , idOfSessionBeingEdited = Nothing
                        , newSessionDate = firstDate
                        , showPreviewUi = False
                        , submissionIdsInputs = [ { id = 1, submissionIds = "", startTime = Nothing, endTime = Nothing } ]
                        , scheduleSubmissionsIndividually = False
                      }
                    , Cmd.none
                    )

            PublishProgrammeBuilder ->
                let
                    newApiPostModel =
                        { datesWithSessions = model.datesWithSessions
                        , tracks = model.tracks
                        , columns = model.columns
                        , locations = model.locations
                        , chairs = model.chairs
                        , published = not model.published
                        }
                in
                    ( { model
                        | published = not model.published
                      }
                    , Api.postModelToDb newApiPostModel model.eventId
                    )

            ToggleNewTrackUi ->
                ( { model
                    | showNewTrackUi = not model.showNewTrackUi
                    , showNewColumnUi = False
                    , showNewSessionUi = False
                    , showManageDatesUi = False
                    , showManageLocationsUi = False
                    , showManageChairsUi = False
                    , idOfSessionBeingEdited = Nothing
                    , showPreviewUi = False
                    , pickedTracks = List.sortBy .name model.tracks
                  }
                , Cmd.none
                )

            CancelAction ->
                ( { model
                    | showNewTrackUi = False
                    , showNewColumnUi = False
                    , showNewSessionUi = False
                    , showManageDatesUi = False
                    , showManageLocationsUi = False
                    , showManageChairsUi = False
                    , idOfSessionBeingEdited = Nothing
                    , showPreviewUi = False
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
                    , showManageLocationsUi = False
                    , showManageChairsUi = False
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
                        , showManageLocationsUi = False
                        , showManageChairsUi = False
                        , showNewColumnUi = False
                        , idOfSessionBeingEdited = Nothing
                        , datePickerClosed = False
                        , pickedDates = List.map .date model.datesWithSessions
                        , showPreviewUi = False
                      }
                    , command
                    )

            ToggleManageLocationsUi ->
                ( { model
                    | showNewColumnUi = False
                    , showNewSessionUi = False
                    , showNewTrackUi = False
                    , showManageDatesUi = False
                    , idOfSessionBeingEdited = Nothing
                    , showManageLocationsUi = not model.showManageLocationsUi
                    , showManageChairsUi = False
                    , showPreviewUi = False
                    , pickedLocations = List.sortBy .name model.locations
                  }
                , Cmd.none
                )

            ToggleManageChairsUi ->
                ( { model
                    | showNewColumnUi = False
                    , showNewSessionUi = False
                    , showNewTrackUi = False
                    , showManageDatesUi = False
                    , idOfSessionBeingEdited = Nothing
                    , showManageLocationsUi = False
                    , showManageChairsUi = not model.showManageChairsUi
                    , showPreviewUi = False
                    , pickedChairs = List.sortBy .name model.chairs
                  }
                , Cmd.none
                )

            ToggleScheduleSubmissionsIndividually ->
                let
                    firstSubmissionIdInput =
                        model.submissionIdsInputs
                            |> List.map .submissionIds
                            |> join ","
                            |> split ","
                            |> List.filterMap (trim >> String.toInt >> Result.toMaybe)
                            |> List.Extra.unique
                            |> List.map toString
                            |> join ", "

                    submissionIdsInputs =
                        [ { id = 1
                          , startTime = Nothing
                          , endTime = Nothing
                          , submissionIds = firstSubmissionIdInput
                          }
                        ]
                in
                    ( { model
                        | scheduleSubmissionsIndividually = not model.scheduleSubmissionsIndividually
                        , submissionIdsInputs = submissionIdsInputs
                      }
                    , Cmd.none
                    )

            UpdateColumns ->
                let
                    newColumns =
                        model.pickedColumns

                    apiPostModel =
                        { datesWithSessions = model.datesWithSessions
                        , tracks = model.tracks
                        , columns = newColumns
                        , locations = model.locations
                        , chairs = model.chairs
                        , published = model.published
                        }
                in
                    ( { model
                        | columns = newColumns
                        , newColumn = blankColumn 1
                        , showValidation = False
                      }
                    , Api.postModelToDb apiPostModel model.eventId
                    )

            CreateNewSession ->
                let
                    highestSessionId =
                        model.datesWithSessions
                            |> List.concatMap .sessions
                            |> List.map .id
                            |> List.maximum
                            |> Maybe.withDefault 0

                    newSessionWithSubmissionIds =
                        addSubmissionIdsInputToSession model.submissionIdsInputs model.newSession model.submissions

                    newSession =
                        { newSessionWithSubmissionIds | id = highestSessionId + 1 }

                    updateDatesWithSessions =
                        List.map updateDateWithSessions model.datesWithSessions

                    updateDateWithSessions dateWithSessions =
                        { dateWithSessions
                            | sessions =
                                if dateWithSessions.date == model.newSessionDate then
                                    newSession :: dateWithSessions.sessions
                                else
                                    dateWithSessions.sessions
                        }

                    apiModelPost =
                        { datesWithSessions = updateDatesWithSessions
                        , tracks = model.tracks
                        , columns = model.columns
                        , locations = model.locations
                        , chairs = model.chairs
                        , published = model.published
                        }
                in
                    ( { model
                        | datesWithSessions = updateDatesWithSessions
                        , newSession = blankSession 1
                        , submissionIdsInputs = [ { submissionIds = "", startTime = Nothing, endTime = Nothing, id = 1 } ]
                        , showValidation = False
                      }
                    , Api.postModelToDb apiModelPost model.eventId
                    )

            UpdateTracks ->
                let
                    newTracks =
                        List.sortBy .name model.pickedTracks

                    apiUpdatePost =
                        ApiUpdatePost model.datesWithSessions newTracks model.locations model.chairs model.columns model.published
                in
                    ( { model
                        | tracks = newTracks
                        , pickedTracks = newTracks
                        , newTrack = blankTrack 1
                        , showValidation = False
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

            SaveModel _ ->
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

            UpdateNewSessionSubmissionIds id startTime endTime newSubmissionIdsString ->
                let
                    validSubmissionIds =
                        model.submissions
                            |> List.map .id
                            |> List.map toString

                    invalidSubmissionIds =
                        newSubmissionIdsString
                            |> split ","
                            |> List.map String.trim
                            |> List.filter (\sub -> not (List.member sub validSubmissionIds))
                            |> join ", "

                    newSubmissionIdsInputs =
                        model.submissionIdsInputs
                            |> addTimeIfNotExists
                            |> List.map setIfHasId

                    addTimeIfNotExists submissionIdsInputs =
                        case List.Extra.find hasId submissionIdsInputs of
                            Nothing ->
                                { submissionIds = ""
                                , startTime = startTime
                                , endTime = endTime
                                , id = generateId submissionIdsInputs
                                }
                                    :: submissionIdsInputs

                            Just _ ->
                                submissionIdsInputs

                    setIfHasId submissionIdsInput =
                        if hasId submissionIdsInput then
                            { submissionIdsInput | submissionIds = newSubmissionIdsString }
                        else
                            submissionIdsInput

                    hasId x =
                        x.id == id
                in
                    ( { model
                        | submissionIdsInputs = newSubmissionIdsInputs
                        , invalidSubmissionIdsInput = invalidSubmissionIds
                      }
                    , Cmd.none
                    )

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

            UpdateNewSessionChair newChairId ->
                case newChairId of
                    Just chairId ->
                        ( (updateNewSession model (\ns -> { ns | chairId = Just chairId })), Cmd.none )

                    Nothing ->
                        ( (updateNewSession model (\ns -> { ns | chairId = Nothing })), Cmd.none )

            UpdateNewSessionLocation newLocationId ->
                case newLocationId of
                    Just locationId ->
                        ( (updateNewSession model (\ns -> { ns | locationId = Just locationId })), Cmd.none )

                    Nothing ->
                        ( (updateNewSession model (\ns -> { ns | locationId = Nothing })), Cmd.none )

            UpdateNewSessionTrack newTrackId ->
                case newTrackId of
                    Just trackId ->
                        ( updateNewSession model (\ns -> { ns | trackId = Just trackId }), Cmd.none )

                    Nothing ->
                        ( updateNewSession model (\ns -> { ns | trackId = Nothing }), Cmd.none )

            UpdateNewSessionDate newDate ->
                case model.idOfSessionBeingEdited of
                    Just id ->
                        ( { model | editSessionDate = DateUtils.valueStringToDateWithoutTime newDate }, Cmd.none )

                    Nothing ->
                        ( { model | newSessionDate = DateUtils.valueStringToDateWithoutTime newDate }, Cmd.none )

            UpdateNewSessionStartHour new ->
                ( updateNewSessionStartTime model (\st -> { st | hour = clamp 0 23 (toInt model new) }), Cmd.none )

            UpdateNewSessionStartMinute new ->
                ( updateNewSessionStartTime model (\st -> { st | minute = clamp 0 59 (toInt model new) }), Cmd.none )

            UpdateNewSessionEndHour new ->
                ( updateNewSessionEndTime model (\et -> { et | hour = clamp 0 23 (toInt model new) }), Cmd.none )

            UpdateNewSessionEndMinute new ->
                ( updateNewSessionEndTime model (\et -> { et | minute = clamp 0 59 (toInt model new) }), Cmd.none )

            ConfirmDeleteSession sessionId ->
                ( model
                , Cmd.batch [ Ports.showDeleteConfirmation sessionId ]
                )

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
                        , locations = model.locations
                        , chairs = model.chairs
                        , published = model.published
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

                    submissionIdsInputs =
                        submissionsToInputText session.submissions

                    scheduleSubmissionsIndividually =
                        List.any
                            (\i -> i.startTime /= Nothing || i.endTime /= Nothing)
                            submissionIdsInputs
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
                        , showManageDatesUi = False
                        , showManageChairsUi = False
                        , showManageLocationsUi = False
                        , editSession = session
                        , submissionIdsInputs = submissionIdsInputs
                        , editSessionDate = sessionDate
                        , scheduleSubmissionsIndividually = scheduleSubmissionsIndividually
                      }
                    , Cmd.none
                    )

            EditSession ->
                case model.idOfSessionBeingEdited of
                    Just id ->
                        let
                            editedSessionWithSubmissionIds =
                                addSubmissionIdsInputToSession model.submissionIdsInputs model.editSession model.submissions

                            updateDatesWithSessions =
                                List.map updateDateWithSessions model.datesWithSessions

                            updateDateWithSessions dateWithSessions =
                                { dateWithSessions
                                    | sessions =
                                        if dateWithSessions.date == model.editSessionDate then
                                            dateWithSessions
                                                |> .sessions
                                                |> List.filter (\s -> s.id /= id)
                                                |> (::) editedSessionWithSubmissionIds
                                        else
                                            dateWithSessions.sessions
                                                |> List.filter (\s -> s.id /= id)
                                }

                            editedSession =
                                { editedSessionWithSubmissionIds
                                    | id = id
                                }

                            apiUpdate =
                                { datesWithSessions = updateDatesWithSessions
                                , tracks = model.tracks
                                , columns = model.columns
                                , published = model.published
                                , locations = model.locations
                                , chairs = model.chairs
                                }
                        in
                            ( { model
                                | datesWithSessions = updateDatesWithSessions
                                , editSession = blankSession 1
                                , showNewSessionUi = False
                                , idOfSessionBeingEdited = Nothing
                                , submissionIdsInputs = [ { submissionIds = "", startTime = Nothing, endTime = Nothing, id = 1 } ]
                              }
                            , Api.postModelToDb apiUpdate model.eventId
                            )

                    Nothing ->
                        ( model, Cmd.none )

            UpdateDates datesList ->
                let
                    datesListToDateWithoutTime =
                        datesList
                            |> List.map DateUtils.pikadayValueToDate

                    allExistingDates =
                        model.datesWithSessions
                            |> List.map .date

                    deletedDates =
                        allExistingDates
                            |> List.filter (\d -> not (List.member d datesListToDateWithoutTime))

                    -- updates dates in datesWithsessions (adds, deletes and updates)
                    datesWithSessionsWithUpdatedDates =
                        datesListToDateWithoutTime
                            |> List.filter (\d -> not (List.member d allExistingDates))
                            |> List.map (\d -> { date = d, sessions = [] })
                            |> List.append model.datesWithSessions
                            |> List.filter (\d -> not (List.member d.date deletedDates))

                    orderedDatesWithSessions =
                        List.sortWith
                            (\a b ->
                                if
                                    (a.date.year >= b.date.year && a.date.month > b.date.month)
                                        || (a.date.year >= b.date.year && a.date.month == b.date.month && a.date.day > b.date.day)
                                then
                                    GT
                                else
                                    LT
                            )
                            datesWithSessionsWithUpdatedDates

                    apiUpdate =
                        { datesWithSessions = orderedDatesWithSessions
                        , tracks = model.tracks
                        , columns = model.columns
                        , published = model.published
                        , locations = model.locations
                        , chairs = model.chairs
                        }
                in
                    ( { model
                        | datesWithSessions = orderedDatesWithSessions
                        , pickedDates = datesListToDateWithoutTime
                      }
                    , Api.postModelToDb apiUpdate model.eventId
                    )

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
                        List.map DateUtils.pikadayValueToDate
                            pickedDatesList
                in
                    ( { model | pickedDates = dateWithoutTimeList }, Cmd.none )

            DeleteDate pickedDateIndex ->
                let
                    -- http://stackoverflow.com/questions/33099945/how-to-remove-an-item-at-a-given-index-from-array-list-in-elm/35871747
                    updatedPickedDates =
                        (List.take pickedDateIndex model.pickedDates) ++ (List.drop (pickedDateIndex + 1) model.pickedDates)
                in
                    ( { model
                        | pickedDates = updatedPickedDates
                      }
                    , Cmd.none
                    )

            GetDateAndThenAddDate id ->
                model ! [ Task.perform (AddNewDate id) Date.now ]

            -- tracks not actually deleted from tracks list until user clicks save changes
            DeleteTrack trackId ->
                let
                    newPickedTracks =
                        List.filter (\t -> t.id /= trackId) model.pickedTracks
                in
                    ( { model | pickedTracks = newPickedTracks }
                    , Cmd.none
                    )

            AddNewTrack ->
                ( { model
                    | pickedTracks =
                        appendNewElementToList model.pickedTracks (Track 0 "" "")
                  }
                , Cmd.none
                )

            UpdatePickedTrack trackId trackField newPickedTrackInput ->
                let
                    pickedTrack =
                        model.pickedTracks
                            |> List.filter (\t -> t.id == trackId)
                            |> List.head
                            |> Maybe.withDefault (Track 0 "" "")

                    newPickedTrack =
                        if trackField == Name then
                            { pickedTrack
                                | name = newPickedTrackInput
                            }
                        else if trackField == Description then
                            { pickedTrack
                                | description = newPickedTrackInput
                            }
                        else
                            pickedTrack

                    --replace track with edited one if already exists
                    newPickedTracks =
                        model.pickedTracks
                            |> List.Extra.replaceIf ((\t -> t.id == trackId)) (newPickedTrack)
                in
                    ( { model | pickedTracks = newPickedTracks }, Cmd.none )

            --  not actually deleted from locations list until user clicks save changes
            DeleteLocation locationId ->
                let
                    newPickedLocations =
                        List.filter (\l -> l.id /= locationId) model.pickedLocations
                in
                    ( { model | pickedLocations = newPickedLocations }
                    , Cmd.none
                    )

            AddNewLocation ->
                ( { model
                    | pickedLocations =
                        appendNewElementToList model.pickedLocations (Location 0 "")
                  }
                , Cmd.none
                )

            UpdatePickedLocation locationId newPickedLocationInput ->
                let
                    pickedLocation =
                        model.pickedLocations
                            |> List.filter (\l -> l.id == locationId)
                            |> List.head
                            |> Maybe.withDefault (Location 0 "")

                    newPickedLocation =
                        { pickedLocation
                            | name = newPickedLocationInput
                        }

                    newPickedLocations =
                        model.pickedLocations
                            |> List.Extra.replaceIf (\s -> s.id == locationId) (newPickedLocation)
                in
                    ( { model | pickedLocations = newPickedLocations }, Cmd.none )

            UpdateLocations ->
                let
                    newLocations =
                        List.sortBy .name model.pickedLocations

                    apiUpdatePost =
                        ApiUpdatePost model.datesWithSessions model.tracks newLocations model.chairs model.columns model.published
                in
                    ( { model
                        | locations = newLocations
                        , pickedLocations = newLocations
                        , newLocation = blankLocation 1
                        , showValidation = False
                      }
                    , Api.postModelToDb apiUpdatePost model.eventId
                    )

            --  not actually deleted from chairs list until user clicks save changes
            DeleteChair chairId ->
                let
                    newPickedChairs =
                        List.filter (\l -> l.id /= chairId) model.pickedChairs
                in
                    ( { model | pickedChairs = newPickedChairs }
                    , Cmd.none
                    )

            AddNewChair ->
                ( { model
                    | pickedChairs =
                        appendNewElementToList model.pickedChairs (Chair 0 "")
                  }
                , Cmd.none
                )

            UpdatePickedChair chairId newPickedChairInput ->
                let
                    pickedChair =
                        model.pickedChairs
                            |> List.filter (\l -> l.id == chairId)
                            |> List.head
                            |> Maybe.withDefault (Chair 0 "")

                    newPickedChair =
                        { pickedChair
                            | name = newPickedChairInput
                        }

                    newPickedChairs =
                        model.pickedChairs
                            |> List.Extra.replaceIf (\s -> s.id == chairId) (newPickedChair)
                in
                    ( { model | pickedChairs = newPickedChairs }, Cmd.none )

            UpdateChairs ->
                let
                    newChairs =
                        List.sortBy .name model.pickedChairs

                    apiUpdatePost =
                        ApiUpdatePost model.datesWithSessions model.tracks model.locations newChairs model.columns model.published
                in
                    ( { model
                        | chairs = newChairs
                        , pickedChairs = newChairs
                        , newChair = blankChair 1
                        , showValidation = False
                      }
                    , Api.postModelToDb apiUpdatePost model.eventId
                    )

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

                    pickedColumnIndex =
                        List.Extra.findIndex (\c -> c.id == columnId) model.pickedColumns
                            |> Maybe.withDefault 0

                    newPickedColumns =
                        List.Extra.setAt pickedColumnIndex newPickedColumn model.pickedColumns
                            |> Maybe.withDefault [ blankColumn 1 ]
                in
                    ( { model
                        | pickedColumns = newPickedColumns
                      }
                    , Cmd.none
                    )

            ShowValidationMessage ->
                ( { model | showValidation = True }, Cmd.none )

            SetSessionSubmissionStartTimes submissionIdsInputId val ->
                let
                    newSubmissionIdsInputs =
                        List.map setStartTimeIfHasId model.submissionIdsInputs

                    setStartTimeIfHasId s =
                        if s.id == submissionIdsInputId then
                            { s | startTime = DateUtils.parseTimeOfDay val }
                        else
                            s
                in
                    ( { model | submissionIdsInputs = newSubmissionIdsInputs }, Cmd.none )

            SetSessionSubmissionEndTimes submissionIdsInputId val ->
                let
                    newSubmissionIdsInputs =
                        List.map setEndTimeIfHasId model.submissionIdsInputs

                    setEndTimeIfHasId s =
                        if s.id == submissionIdsInputId then
                            { s | endTime = DateUtils.parseTimeOfDay val }
                        else
                            s
                in
                    ( { model | submissionIdsInputs = newSubmissionIdsInputs }, Cmd.none )

            CreateSubmissionInput ->
                let
                    newModel =
                        ({ model
                            | submissionIdsInputs =
                                model.submissionIdsInputs
                                    ++ [ { submissionIds = ""
                                         , startTime = Nothing
                                         , endTime = Nothing
                                         , id = generateId model.submissionIdsInputs
                                         }
                                       ]
                         }
                        )
                in
                    ( newModel, Cmd.none )

            DeleteSubmissionInput id ->
                let
                    newModel =
                        { model
                            | submissionIdsInputs =
                                List.filter (.id >> (/=) id) model.submissionIdsInputs
                        }
                in
                    ( newModel, Cmd.none )

            MoveColumnUp columnIndex ->
                let
                    newPickedColumns =
                        model.pickedColumns
                            |> List.Extra.swapAt columnIndex (columnIndex - 1)
                            |> Maybe.withDefault []
                in
                    ( { model | pickedColumns = newPickedColumns }, Cmd.none )

            MoveColumnDown columnIndex ->
                let
                    newPickedColumns =
                        model.pickedColumns
                            |> List.Extra.swapAt columnIndex (columnIndex + 1)
                            |> Maybe.withDefault []
                in
                    ( { model | pickedColumns = newPickedColumns }, Cmd.none )


updateSessionSubmissions : Model -> Int -> List Int -> (SessionSubmission -> SessionSubmission) -> Model
updateSessionSubmissions model sessionId submissionIds update =
    let
        updateDateWithSessions dws =
            { dws | sessions = List.map updateSession dws.sessions }

        updateSession s =
            if s.id == sessionId then
                { s | submissions = List.map updateSubmission s.submissions }
            else
                s

        updateSubmission sub =
            if List.member sub.id submissionIds then
                update sub
            else
                sub
    in
        { model | datesWithSessions = List.map updateDateWithSessions model.datesWithSessions }


generateId =
    List.map .id
        >> List.maximum
        >> Maybe.withDefault 0
        >> (+) 1
