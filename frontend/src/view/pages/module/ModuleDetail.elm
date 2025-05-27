module View.Pages.Module.ModuleDetail exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, h1, h2, h3, text, ul, li, p, h5, span, table, thead, tbody, tr, th, td)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, stopPropagationOn)
import Http
import Shared.Models.Module exposing (Module)
import Shared.Models.Exam exposing (Exam)
import Shared.Services.ModuleService as ModuleService
import Shared.Services.ExamService as ExamService
import Shared.Services.RoundService as Round
import Dict exposing (Dict)
import Browser.Navigation as Nav
import Json.Decode as Decode
import View.Pages.Exam.ExamAdd as ExamAdd
import View.Pages.Exam.ExamEdit as ExamEdit

type alias Model =
    { moduleId : Int
    , moduleData : Maybe Module
    , exams : List Exam
    , loading : Bool
    , error : Maybe String
    , navKey : Nav.Key
    , editingExam : Maybe ExamEdit.Model
    , addingExam : Maybe ExamAdd.Model
    }

type Msg
    = ModuleLoaded (Result Http.Error Module)
    | ExamsLoaded (Result Http.Error (List Exam))
    | EditExam Int
    | DeleteExam Int
    | ExamDeleted Int (Result Http.Error ())
    | AddExam
    | ExamEditMsg ExamEdit.Msg
    | ExamAddMsg ExamAdd.Msg

init : Int -> Nav.Key -> ( Model, Cmd Msg )
init moduleId key =
    ( { moduleId = moduleId
      , moduleData = Nothing
      , exams = []
      , loading = True
      , error = Nothing
      , navKey = key
      , editingExam = Nothing
      , addingExam = Nothing
      }
    , Cmd.batch
        [ ModuleService.getById moduleId ModuleLoaded
        , ExamService.getExamsFromModuleId moduleId ExamsLoaded
        ]
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModuleLoaded result ->
            case result of
                Ok moduleData ->
                    ( { model | moduleData = Just moduleData, loading = False }, Cmd.none )

                Err _ ->
                    ( { model | loading = False, error = Just "Failed to load module details" }, Cmd.none )

        ExamsLoaded result ->
            case result of
                Ok exams ->
                    ( { model | exams = exams, loading = False }, Cmd.none )

                Err _ ->
                    ( { model | loading = False, error = Just "Failed to load exams" }, Cmd.none )

        EditExam examId ->
            let
                (editModel, editCmd) = ExamEdit.init model.moduleId examId
            in
            ( { model | editingExam = Just editModel, addingExam = Nothing }
            , Cmd.map ExamEditMsg editCmd
            )

        DeleteExam examId ->
            (model, ExamService.delete examId (ExamDeleted examId))

        ExamDeleted examId (Ok _) ->
            ( { model | exams = List.filter (\e -> e.id /= examId) model.exams }
            , Cmd.none
            )

        ExamDeleted _ (Err _) ->
            ( { model | error = Just "Löschen der Prüfung fehlgeschlagen." }
            , Cmd.none
            )

        AddExam ->
            ( { model | addingExam = Just (ExamAdd.init model.moduleId), editingExam = Nothing }
            , Cmd.none
            )

        ExamEditMsg subMsg ->
            case model.editingExam of
                Just editModel ->
                    let
                        (newEditModel, editCmd) = ExamEdit.update subMsg editModel
                    in
                    case subMsg of
                        ExamEdit.Cancel ->
                            ( { model | editingExam = Nothing }, Cmd.none )

                        ExamEdit.ExamUpdated (Ok exam) ->
                            ( { model 
                              | editingExam = Nothing
                              , exams = List.map (\e -> if e.id == exam.id then exam else e) model.exams 
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( { model | editingExam = Just newEditModel }
                            , Cmd.map ExamEditMsg editCmd
                            )

                Nothing ->
                    (model, Cmd.none)

        ExamAddMsg subMsg ->
            case model.addingExam of
                Just addModel ->
                    let
                        (newAddModel, addCmd) = ExamAdd.update subMsg addModel
                    in
                    case subMsg of
                        ExamAdd.Cancel ->
                            ( { model | addingExam = Nothing }, Cmd.none )

                        ExamAdd.ExamAdded (Ok _) ->
                            ( { model | addingExam = Nothing, loading = True }
                            , ExamService.getExamsFromModuleId model.moduleId ExamsLoaded
                            )

                        _ ->
                            ( { model | addingExam = Just newAddModel }
                            , Cmd.map ExamAddMsg addCmd
                            )
                Nothing ->
                    (model, Cmd.none)

calculateModuleAverage : List Exam -> Float
calculateModuleAverage exams =
    if List.isEmpty exams then
        0
    else
        let
            totalWeightedGrade = List.foldl (\exam acc -> acc + (exam.grade * exam.weight)) 0 exams
            totalWeight = List.foldl (\exam acc -> acc + exam.weight) 0 exams
        in
        if totalWeight > 0 then
            totalWeightedGrade / totalWeight
        else
            0

formatGrade : Float -> String
formatGrade grade =
    if grade == 0 then
        "-"
    else
        Round.round 2 grade

view : Model -> Html Msg
view model =
    case model.editingExam of
        Just editModel ->
            Html.map ExamEditMsg (ExamEdit.view editModel)
        
        Nothing ->
            case model.addingExam of
                Just addModel ->
                    Html.map ExamAddMsg (ExamAdd.view addModel)
                
                Nothing ->
                    div [ class "container pt-4" ]
                        ([ h1 [ class "mb-4" ] [ text ("Modul Detail: " ++ String.fromInt model.moduleId) ] ]
                            ++ (if model.loading then
                                    [ div [ class "text-center" ] [ text "Loading module..." ] ]
                                else
                                    case model.error of
                                        Just msg ->
                                            [ div [ class "alert alert-danger" ] [ text msg ] ]
                                        Nothing ->
                                            [ moduleSummary model
                                            , div [ class "d-flex justify-content-between align-items-center mt-4 mb-3" ]
                                                [ h2 [ class "mb-0" ] [ text "Prüfungen:" ]
                                                , button
                                                    [ class "btn btn-primary"
                                                    , onClick AddExam
                                                    ]
                                                    [ text "Add Exam" ]
                                                ]
                                            , div [ class "row g-3" ]
                                                (List.map examCard model.exams)
                                            ]
                                )
                        )

moduleSummary : Model -> Html Msg
moduleSummary model =
    let
        average = calculateModuleAverage model.exams
    in
    div [ class "mt-5 p-4 bg-light rounded" ]
        [ div [ class "card-body" ]
            [ h2 [ class "card-title h4" ] [ text "Modul Übersicht" ]
            , div [ class "row" ]
                [ div [ class "col-md-6" ]
                    [ p [ class "mb-2" ] 
                        [ text "Durchschnitt: "
                        , span 
                            [ class "fw-bold"
                            , style "color" (if average >= 4.0 then "green" else "red")
                            ] 
                            [ text (formatGrade average) ]
                        ]
                    ]
                , div [ class "col-md-6" ]
                    [ p [ class "mb-2" ] 
                        [ text "Anzahl Prüfungen: "
                        , span [ class "fw-bold" ] [ text (String.fromInt (List.length model.exams)) ]
                        ]
                    ]
                ]
            ]
        ]

examCard : Exam -> Html Msg
examCard exam =
    div [ class "col-md-4" ]
        [ div [ class "card exam-card" ]
            [ div [ class "card-body position-relative" ]
                [ h5 [ class "card-title" ] [ text exam.name ]
                , p [ class "card-text" ]
                    [ text "Note: "
                    , span 
                        [ class "fw-bold"
                        , style "color" (if exam.grade >= 4.0 then "green" else "red")
                        ] 
                        [ text (formatGrade exam.grade) ]
                    ]
                , p [ class "card-text" ] 
                    [ text ("Gewichtung: " ++ String.fromFloat exam.weight ++ "%") ]
                , div [ class "position-absolute top-0 end-0 p-2 d-hover-flex gap-2" ]
                    [ button
                        [ class "btn btn-sm btn-outline-secondary me-2"
                        , onClick (EditExam exam.id)
                        ]
                        [ text "Edit" ]
                    , button
                        [ class "btn btn-sm btn-outline-danger"
                        , onClick (DeleteExam exam.id)
                        ]
                        [ text "Delete" ]
                    ]
                ]
            ]
        ]