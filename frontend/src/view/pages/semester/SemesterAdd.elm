module View.Pages.Semester.SemesterAdd exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, h1, input, label, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)
import Shared.Models.Semester exposing (Semester)
import Shared.Services.SemesterService as SemesterService
import Http

type Msg
    = NameChanged String
    | Submit
    | SubmitResult (Result Http.Error Semester)
    | Cancel

type alias Model =
    { name : String
    , isSubmitting : Bool
    , error : Maybe String
    }

init : Model
init =
    { name = ""
    , isSubmitting = False
    , error = Nothing
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NameChanged newName ->
            ( { model | name = newName, error = Nothing }, Cmd.none )

        Submit ->
            let
                newSemester =
                    { id = 0
                    , name = model.name
                    }
            in
            ( { model | isSubmitting = True, error = Nothing }
            , SemesterService.create newSemester SubmitResult
            )

        SubmitResult result ->
            case result of
                Ok _ ->
                    update Cancel model

                Err _ ->
                    ( { model | isSubmitting = False, error = Just "Failed to create semester." }
                    , Cmd.none
                    )

        Cancel ->
            (model, Cmd.none)

view : Model -> Html Msg
view model =
    div [ class "container pt-4" ]
        [ div [ class "card shadow-sm" ]
            [ div [ class "card-header" ]
                [ h1 [ class "h5 mb-0" ] [ text "Add Semester" ] ]
            , div [ class "card-body" ]
                [ div [ class "mb-3" ]
                    [ label [ class "form-label" ] [ text "Semester Name" ]
                    , input
                        [ class "form-control"
                        , type_ "text"
                        , value model.name
                        , onInput NameChanged
                        ]
                        []
                    ]
                , case model.error of
                    Just msg ->
                        div [ class "text-danger mt-2" ] [ text msg ]

                    Nothing ->
                        text ""
                ]
            , div [ class "card-footer d-flex justify-content-end gap-2" ]
                [ button [ class "btn btn-secondary", onClick Cancel ] [ text "Cancel" ]
                , button
                    [ class "btn btn-primary"
                    , onClick Submit
                    , Html.Attributes.disabled model.isSubmitting
                    ]
                    [ text (if model.isSubmitting then "Creating..." else "Create") ]
                ]
            ]
        ]
