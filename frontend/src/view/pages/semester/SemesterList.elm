module View.Pages.Semester.SemesterList exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, div, h1, li, text, ul, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
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
    | NavigateToEdit Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NavigateToAdd ->
            (model, Cmd.none)
        NavigateToEdit id ->
            (model, Cmd.none)

view : Model -> Html Msg
view model =
    div [ class "container pt-4" ]
        [   
            div [ class "d-flex" ] [
                h1 [] [ text "Semester List" ],
                div [ class "ms-auto" ] [
                    button [ class "btn btn-primary", onClick NavigateToAdd ] [ text "Add Semester" ]
                ]
            ],
            ul []
                (List.map
                    (\semester ->
                        li [] [ text semester.name ]
                    )
                    model.semesters
                )
        ]