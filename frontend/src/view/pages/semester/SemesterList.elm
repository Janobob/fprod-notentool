module View.Pages.Semester.SemesterList exposing (Model, Msg, init, update, view)

import Html exposing (Html, div, h1, li, text, ul)
import Html.Attributes exposing (class)
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
    = NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)

view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "col" ]
                [ h1 [] [ text "Semester List" ]
                , ul []
                    (List.map
                        (\semester ->
                            li [] [ text semester.name ]
                        )
                        model.semesters
                    )
                ]
            ]
        ]