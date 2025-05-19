module View.Pages.Semester.SemesterList exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, h1, p, text, h5)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, stopPropagationOn)
import Http
import Json.Decode as Decode
import Shared.Models.Semester exposing (Semester)
import Shared.Services.SemesterService as SemesterService

type alias Model =
    { semesters : List Semester
    , loading : Bool
    , error : Maybe String
    }

init : (Model, Cmd Msg)
init =
    ( { semesters = [], loading = True, error = Nothing }
    , SemesterService.getAll SemestersLoaded
    )

type Msg
    = NavigateToAdd
    | NavigateToDetail Int
    | NavigateToEdit Int
    | NavigateToDelete Int
    | SemestersLoaded (Result Http.Error (List Semester))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NavigateToAdd ->
            (model, Cmd.none)
        NavigateToDetail id ->
            (model, Cmd.none)
        NavigateToEdit id ->
            (model, Cmd.none)
        NavigateToDelete id ->
            (model, Cmd.none)

        SemestersLoaded result ->
            case result of
                Ok semesters ->
                    ( { model | semesters = semesters, loading = False }, Cmd.none )

                Err _ ->
                    ( { model | loading = False, error = Just "Failed to load semesters." }, Cmd.none )

view : Model -> Html Msg
view model =
    if model.loading then
        div [ class "pt-4 text-center" ] [ text "Loading..." ]

    else
        div [ class "container pt-4" ]
            ([ div [ class "d-flex mb-4 align-items-center" ]
                [ h1 [ class "mb-0" ] [ text "Semester List" ]
                , div [ class "ms-auto" ]
                    [ button [ class "btn btn-primary", onClick NavigateToAdd ]
                        [ text "Add Semester" ]
                    ]
                ]
            ]
            ++ errorAlert model.error
            ++ emptyAlert model.semesters
            ++ [ div [ class "row g-3" ]
                    (List.map semesterCard model.semesters)
                ]
            )

errorAlert : Maybe String -> List (Html msg)
errorAlert maybeError =
    case maybeError of
        Just msg ->
            [ div [ class "alert alert-danger mt-3" ]
                [ text msg ]
            ]

        Nothing ->
            []

emptyAlert : List Semester -> List (Html msg)
emptyAlert semesters =
    if List.isEmpty semesters then
        [ div [ class "alert alert-info mt-3"]
            [ text "You haven't added any semesters yet." ]
        ]
    else
        []


semesterCard : Semester -> Html Msg
semesterCard semester =
    div [ class "col-md-4" ]
        [ div [ class "card semester-card", onClick (NavigateToDetail semester.id) ]
            [ div [ class "card-body position-relative" ]
                [ h5 [ class "card-title" ] [ text semester.name ]
                , p [ class "card-text" ] [ text "Gesamtnote: - (todo)" ]
                , p [ class "card-text" ] [ text "Module: - (todo)" ]
                , div [ class "position-absolute top-0 end-0 p-2 d-hover-flex gap-2" ]
                    [ button 
                        [ class "btn btn-sm btn-outline-secondary"
                        , stopPropagationOn "click" (Decode.succeed ( NavigateToEdit semester.id, True ))
                        ]
                        [ text "Edit" ]
                    , button 
                        [ class "btn btn-sm btn-outline-danger"
                        , stopPropagationOn "click" (Decode.succeed ( NavigateToDelete semester.id, True ))
                        ]
                        [ text "Delete" ]
                    ]
                ]
            ]
        ]