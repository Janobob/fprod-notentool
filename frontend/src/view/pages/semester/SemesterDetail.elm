module View.Pages.Semester.SemesterDetail exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, h1, h2, h3, text, ul, li, p, h5, span, table, thead, tbody, tr, th, td)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Http
import Shared.Models.Module exposing (Module)
import Shared.Models.Exam exposing (Exam)
import Shared.Services.SemesterService as SemesterService
import Shared.Services.ExamService as ExamService
import Dict exposing (Dict)
import Shared.Services.RoundService as Round

type alias ModuleWithExams =
    { module_ : Module
    , exams : List Exam
    , loading : Bool
    , error : Maybe String
    }

type alias Model =
    { semesterId : Int
    , modules : List ModuleWithExams
    , loading : Bool
    , error : Maybe String
    }

type Msg
    = ModulesLoaded (Result Http.Error (List Module))
    | ExamsLoaded Int (Result Http.Error (List Exam))
    | NavigateToModuleDetail Int
    | EditModule Int
    | DeleteModule Int

init : Int -> (Model, Cmd Msg)
init semesterId =
    ( { semesterId = semesterId, modules = [], loading = True, error = Nothing }
    , SemesterService.getModulesFromSemesterId semesterId ModulesLoaded
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ModulesLoaded result ->
            case result of
                Ok modules ->
                    let
                        modulesWithExams = List.map (\m -> 
                            { module_ = m
                            , exams = []
                            , loading = True
                            , error = Nothing
                            }) modules
                        
                        loadExamsCommands = List.map (\m -> 
                            ExamService.getExamsFromModuleId m.id (ExamsLoaded m.id)) modules
                    in
                    ( { model | modules = modulesWithExams, loading = False }
                    , Cmd.batch loadExamsCommands
                    )

                Err _ ->
                    ( { model | loading = False, error = Just "Failed to load modules." }, Cmd.none )

        ExamsLoaded moduleId result ->
            let
                updateModule mwe =
                    if mwe.module_.id == moduleId then
                        case result of
                            Ok exams ->
                                { mwe | exams = exams, loading = False }
                            Err _ ->
                                { mwe | loading = False, error = Just "Failed to load exams." }
                    else
                        mwe
            in
            ( { model | modules = List.map updateModule model.modules }, Cmd.none )

        NavigateToModuleDetail id ->
            (model, Cmd.none)
    
        EditModule id ->
            (model, Cmd.none)

        DeleteModule id ->
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

calculateSemesterAverage : List ModuleWithExams -> Float
calculateSemesterAverage modulesWithExams =
    let
        moduleAverages = List.map (\mwe -> calculateModuleAverage mwe.exams) modulesWithExams
        validAverages = List.filter (\avg -> avg > 0) moduleAverages
    in
    if List.isEmpty validAverages then
        0
    else
        List.sum validAverages / (toFloat (List.length validAverages))

formatGrade : Float -> String
formatGrade grade =
    if grade == 0 then
        "-"
    else
        Round.round 2 grade

view : Model -> Html Msg
view model =
    div [ class "container pt-4" ]
        ([ h1 [ class "mb-4" ] [ text ("Semester Detail: " ++ String.fromInt model.semesterId) ] ]
            ++ (if model.loading then
                    [ div [ class "text-center" ] [ text "Loading modules..." ] ]

                else
                    case model.error of
                        Just msg ->
                            [ div [ class "alert alert-danger" ] [ text msg ] ]

                        Nothing ->
                            [ semesterSummary model.modules
                            ,h2 [ class "mt-4" ] [ text "Module:" ]
                            , div [ class "row g-3" ]
                                (List.map moduleCard model.modules)
                            ]
               )
        )

moduleCard : ModuleWithExams -> Html Msg
moduleCard moduleWithExams =
    let
        module_ = moduleWithExams.module_
        average = calculateModuleAverage moduleWithExams.exams
    in
    div [ class "col-md-4" ]
        [ div [ class "card module-card", onClick (NavigateToModuleDetail module_.id) ]
            [ div [ class "card-body position-relative" ]
                [ h5 [ class "card-title" ] [ text (module_.abbreviation ++ ": " ++ module_.name) ]
                , p [ class "card-text" ] 
                    [ text "Durchschnitt: "
                    , span [ class "fw-bold" ] [ text (formatGrade average) ]
                    ]
                , if not (List.isEmpty moduleWithExams.exams) then
                    div [ class "mt-3" ]
                        [ h3 [ class "h6" ] [ text "Prüfungen:" ]
                        , table [ class "table table-sm" ]
                            [ thead []
                                [ tr []
                                    [ th [] [ text "Name" ]
                                    , th [] [ text "Note" ]
                                    , th [] [ text "Gewichtung" ]
                                    ]
                                ]
                            , tbody []
                                (List.map examRow moduleWithExams.exams)
                            ]
                        ]
                  else
                    text ""
                , div [ class "position-absolute top-0 end-0 p-2 d-hover-flex gap-2" ]
                    [ button
                        [ class "btn btn-sm btn-outline-secondary"
                        , onClick (EditModule module_.id)
                        ]
                        [ text "Edit" ]
                    , button
                        [ class "btn btn-sm btn-outline-danger"
                        , onClick (DeleteModule module_.id)
                        ]
                        [ text "Delete" ]
                    ]
                ]
            ]
        ]

examRow : Exam -> Html Msg
examRow exam =
    tr []
        [ td [] [ text exam.name ]
        , td [] [ text (String.fromFloat exam.grade) ]
        , td [] [ text (String.fromFloat exam.weight) ]
        ]

semesterSummary : List ModuleWithExams -> Html Msg
semesterSummary modules =
    let
        semesterAverage = calculateSemesterAverage modules
    in
    div [ class "mt-5 p-4 bg-light rounded" ]
        [ h2 [ class "mb-3" ] [ text "Semester Zusammenfassung" ]
        , div [ class "row" ]
            [ div [ class "col-md-6" ]
                [ h3 [ class "h5" ] [ text "Module Übersicht:" ]
                , table [ class "table" ]
                    [ thead []
                        [ tr []
                            [ th [] [ text "Modul" ]
                            , th [] [ text "Durchschnitt" ]
                            ]
                        ]
                    , tbody []
                        (List.map (\mwe -> 
                            tr []
                                [ td [] [ text mwe.module_.abbreviation ]
                                , td [] [ text (formatGrade (calculateModuleAverage mwe.exams)) ]
                                ]
                        ) modules)
                    ]
                ]
            , div [ class "col-md-6" ]
                [ div [ class "p-4 border rounded text-center" ]
                    [ h3 [ class "h5 mb-3" ] [ text "Semester Durchschnitt:" ]
                    , span 
                        [ class "display-4"
                        , style "color" (if semesterAverage >= 4.0 then "green" else "red")
                        ] 
                        [ text (formatGrade semesterAverage) ]
                    ]
                ]
            ]
        ]