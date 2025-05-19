module View.Pages.Semester.SemesterList exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, div, h1, li, text, ul, button, p, h5)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode as Decode exposing (Decoder)
import Shared.Models.Semester exposing (Semester)

type alias Model
    = { semesters : List Semester }

init : Model
init =
    { semesters =
        [ { id = 1, name = "HS25" }
        , { id = 2, name = "FS25" }
        ]
    }

type Msg
    = NavigateToAdd
    | NavigateToDetail Int
    | NavigateToEdit Int
    | NavigateToDelete Int
    | NoOp

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
        NoOp ->
            (model, Cmd.none)

view : Model -> Html Msg
view model =
    div [ class "container pt-4" ]
        [   
            div [ class "d-flex mb-4 align-items-center" ] 
                [ h1 [] [ text "Semester List" ]
                , div [ class "ms-auto" ] 
                    [ button [ class "btn btn-primary", onClick NavigateToAdd ] [ text "Add Semester" ] ]
                ]
        , div [ class "row g-3" ]
            (List.map semesterCard model.semesters)
        ]


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