module View.Pages.Semester.SemesterAdd exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, h1, input, label, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)

type Msg
    = NameChanged String
    | Submit
    | Cancel

type alias Model =
    { name : String }

init : Model
init =
    { name = "" }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NameChanged newName ->
            ( { model | name = newName }, Cmd.none )

        Submit ->
            -- todo: send the new semester name to the backend
            (model, Cmd.none)

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
                ]
            , div [ class "card-footer d-flex justify-content-end gap-2" ]
                [ button [ class "btn btn-secondary", onClick Cancel ] [ text "Cancel" ]
                , button [ class "btn btn-primary", onClick Submit ] [ text "Create" ]
                ]
            ]
        ]