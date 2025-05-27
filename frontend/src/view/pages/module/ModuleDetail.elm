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

type alias Model =
    { moduleId : Int
    , moduleData : Maybe Module
    , exams : List Exam
    , loading : Bool
    , error : Maybe String
    , navKey : Nav.Key
    }

type Msg
    = ModuleLoaded (Result Http.Error Module)
    | ExamsLoaded (Result Http.Error (List Exam))
    | EditExam Int
    | DeleteExam Int
    | ExamDeleted Int (Result Http.Error ())
    | AddExam

init : Int -> Nav.Key -> ( Model, Cmd Msg )
init moduleId key =
    ( { moduleId = moduleId
      , moduleData = Nothing
      , exams = []
      , loading = True
      , error = Nothing
      , navKey = key
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

        EditExam id ->
            -- TODO: Implement exam editing
            (model, Cmd.none)

        DeleteExam id ->
            (model, ExamService.delete id (ExamDeleted id))

        ExamDeleted id (Ok _) ->
            let
                filteredExams = List.filter (\exam -> exam.id /= id) model.exams
            in
            ( { model | exams = filteredExams }, Cmd.none )

        ExamDeleted _ (Err _) ->
            ( { model | error = Just "Löschen der Prüfung fehlgeschlagen." }, Cmd.none )

        AddExam ->
            -- TODO: Implement exam adding
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
    case model.moduleData of
        Just m ->
            let
                average = calculateModuleAverage model.exams
            in
            div [ class "mt-5 p-4 bg-light rounded" ]
                [ h2 [ class "mb-3" ] [ text "Modul Zusammenfassung" ]
                , div [ class "row" ]
                    [ div [ class "col-md-6" ]
                        [ h3 [ class "h5" ] [ text "Modul Übersicht:" ]
                        , table [ class "table" ]
                            [ thead []
                                [ tr []
                                    [ th [] [ text "Kürzel" ]
                                    , th [] [ text "Name" ]
                                    , th [] [ text "ID" ]
                                    ]
                                ]
                            , tbody []
                                [ tr []
                                    [ td [] [ text m.abbreviation ]
                                    , td [] [ text m.name ]
                                    , td [] [ text (String.fromInt m.id) ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "col-md-6" ]
                        [ div [ class "p-4 border rounded text-center" ]
                            [ h3 [ class "h5 mb-3" ] [ text "Modul Durchschnitt:" ]
                            , span
                                [ class "display-4"
                                , style "color" (if average >= 4.0 then "green" else "red")
                                ]
                                [ text (formatGrade average) ]
                            ]
                        ]
                    ]
                ]
        Nothing ->
            text ""

examCard : Exam -> Html Msg
examCard exam =
    div [ class "col-md-4" ]
        [ div [ class "card exam-card" ]
            [ div [ class "card-body position-relative" ]
                [ h5 [ class "card-title" ] [ text exam.name ]
                , p [ class "card-text" ]
                    [ text "Note: "
                    , span [ class "fw-bold" ] [ text (formatGrade exam.grade) ]
                    , text (" | Gewichtung: " ++ String.fromFloat exam.weight ++ "%")
                    ]
                , div [ class "position-absolute top-0 end-0 p-2 d-hover-flex gap-2" ]
                    [ button
                        [ class "btn btn-sm btn-outline-secondary me-2"
                        , stopPropagationOn "click" (Decode.succeed ( EditExam exam.id, True ))
                        ]
                        [ text "Edit" ]
                    , button
                        [ class "btn btn-sm btn-outline-danger"
                        , stopPropagationOn "click" (Decode.succeed ( DeleteExam exam.id, True ))
                        ]
                        [ text "Delete" ]
                    ]
                ]
            ]
        ] 