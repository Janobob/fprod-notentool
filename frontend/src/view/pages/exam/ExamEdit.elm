module View.Pages.Exam.ExamEdit exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, form, h1, input, label, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Shared.Models.Exam exposing (Exam)
import Shared.Services.ExamService as ExamService

type alias Model =
    { moduleId : Int
    , examId : Int
    , name : String
    , grade : String
    , weight : String
    , loading : Bool
    , error : Maybe String
    , successMessage : Maybe String
    }

type Msg
    = UpdateName String
    | UpdateGrade String
    | UpdateWeight String
    | Submit
    | Cancel
    | ExamLoaded (Result Http.Error Exam)
    | ExamUpdated (Result Http.Error Exam)

init : Int -> Int -> (Model, Cmd Msg)
init moduleId examId =
    ( { moduleId = moduleId
      , examId = examId
      , name = ""
      , grade = ""
      , weight = ""
      , loading = True
      , error = Nothing
      , successMessage = Nothing
      }
    , ExamService.getById examId ExamLoaded
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpdateName name ->
            ( { model | name = name, successMessage = Nothing }, Cmd.none )

        UpdateGrade grade ->
            ( { model | grade = grade, successMessage = Nothing }, Cmd.none )

        UpdateWeight weight ->
            ( { model | weight = weight, successMessage = Nothing }, Cmd.none )

        Submit ->
            case (String.toFloat model.grade, String.toFloat model.weight) of
                (Just grade, Just weight) ->
                    if model.name == "" then
                        ( { model | error = Just "Name darf nicht leer sein" }, Cmd.none )
                    else if grade < 1.0 || grade > 6.0 then
                        ( { model | error = Just "Note muss zwischen 1.0 und 6.0 liegen" }, Cmd.none )
                    else if weight <= 0 || weight > 100 then
                        ( { model | error = Just "Gewichtung muss zwischen 0 und 100 liegen" }, Cmd.none )
                    else
                        let
                            exam =
                                { id = model.examId
                                , name = model.name
                                , grade = grade
                                , weight = weight
                                , moduleId = model.moduleId
                                }
                        in
                        ( { model | loading = True, error = Nothing }
                        , ExamService.update model.examId exam ExamUpdated
                        )
                _ ->
                    ( { model | error = Just "Ungültige Eingabe für Note oder Gewichtung" }, Cmd.none )

        Cancel ->
            (model, Cmd.none)

        ExamLoaded (Ok exam) ->
            ( { model 
              | name = exam.name
              , grade = String.fromFloat exam.grade
              , weight = String.fromFloat exam.weight
              , loading = False
              }
            , Cmd.none
            )

        ExamLoaded (Err _) ->
            ( { model 
              | loading = False
              , error = Just "Fehler beim Laden der Prüfung"
              }
            , Cmd.none
            )

        ExamUpdated (Ok _) ->
            ( { model 
              | loading = False
              , successMessage = Just "Prüfung erfolgreich aktualisiert"
              }
            , ExamService.getById model.examId ExamLoaded
            )

        ExamUpdated (Err _) ->
            ( { model 
              | loading = False
              , error = Just "Fehler beim Aktualisieren der Prüfung"
              }
            , Cmd.none
            )

view : Model -> Html Msg
view model =
    div [ class "container pt-4" ]
        [ div [ class "card shadow-sm" ]
            [ div [ class "card-header" ]
                [ h1 [ class "h5 mb-0" ] [ text ("Prüfung bearbeiten #" ++ String.fromInt model.examId) ] ]
            , div [ class "card-body" ]
                [ div [ class "card-body" ]
                    [ div [ class "mb-3" ]
                        [ label [ class "form-label" ] [ text "Name" ]
                        , input
                            [ type_ "text"
                            , class "form-control"
                            , value model.name
                            , onInput UpdateName
                            ]
                            []
                        ]
                    , div [ class "mb-3" ]
                        [ label [ class "form-label" ] [ text "Note" ]
                        , input
                            [ type_ "number"
                            , class "form-control"
                            , value model.grade
                            , onInput UpdateGrade
                            ]
                            []
                        ]
                    , div [ class "mb-3" ]
                        [ label [ class "form-label" ] [ text "Gewichtung (%)" ]
                        , input
                            [ type_ "number"
                            , class "form-control"
                            , value model.weight
                            , onInput UpdateWeight
                            ]
                            []
                        ]
                    ]
                , case model.error of
                    Just error ->
                        div [ class "alert alert-danger" ] [ text error ]
                    Nothing ->
                        text ""
                ]
            , div [ class "card-footer d-flex justify-content-end gap-2" ]
                [ button
                    [ type_ "button"
                    , class "btn btn-secondary"
                    , onClick Cancel
                    ]
                    [ text "Cancel" ]
                , button
                    [ type_ "submit"
                    , class "btn btn-primary"
                    , onClick Submit
                    ]
                    [ text "Save" ]
                ]
            ]
        ]