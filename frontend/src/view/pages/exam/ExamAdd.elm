module View.Pages.Exam.ExamAdd exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, form, h1, input, label, text)
import Html.Attributes exposing (class, type_, value, disabled)
import Html.Events exposing (onClick, onInput)
import Http
import Shared.Models.Exam exposing (Exam)
import Shared.Services.ExamService as ExamService

type alias Model =
    { moduleId : Int
    , name : String
    , grade : String
    , weight : String
    , error : Maybe String
    , isSubmitting : Bool
    }

type Msg
    = UpdateName String
    | UpdateGrade String
    | UpdateWeight String
    | Submit
    | Cancel
    | ExamAdded (Result Http.Error Exam)

init : Int -> Model
init moduleId =
    { moduleId = moduleId
    , name = ""
    , grade = ""
    , weight = ""
    , error = Nothing
    , isSubmitting = False
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpdateName name ->
            ( { model | name = name, error = Nothing }, Cmd.none )

        UpdateGrade grade ->
            ( { model | grade = grade, error = Nothing }, Cmd.none )

        UpdateWeight weight ->
            ( { model | weight = weight, error = Nothing }, Cmd.none )

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
                                { id = 0
                                , name = model.name
                                , grade = grade
                                , weight = weight
                                , moduleId = model.moduleId
                                }
                        in
                        ( { model | isSubmitting = True, error = Nothing }
                        , ExamService.create model.moduleId exam ExamAdded
                        )
                _ ->
                    ( { model | error = Just "Ungültige Eingabe für Note oder Gewichtung" }, Cmd.none )

        Cancel ->
            (model, Cmd.none)

        ExamAdded (Ok _) ->
            update Cancel { model | isSubmitting = False }

        ExamAdded (Err _) ->
            ( { model | error = Just "Fehler beim Speichern der Prüfung", isSubmitting = False }, Cmd.none )

view : Model -> Html Msg
view model =
    div [ class "container pt-4" ]
        [ div [ class "card shadow-sm" ]
            [ div [ class "card-header" ]
                [ h1 [ class "h5 mb-0" ] [ text "Prüfung hinzufügen" ] ]
            , div [ class "card-body" ]
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
                    [ type_ "button"
                    , class "btn btn-primary"
                    , onClick Submit
                    , disabled model.isSubmitting
                    ]
                    [ text (if model.isSubmitting then "Creating..." else "Create") ]
                ]
            ]
        ]