module View.Pages.Semester.SemesterDelete exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

type Msg
    = ConfirmDelete
    | Cancel

type alias Model =
    { id : Int }

init : Int -> (Model, Cmd Msg)
init id =
    ( { id = id }, Cmd.none )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ConfirmDelete ->
            -- todo: send DELETE request
            (model, Cmd.none)

        Cancel ->
            (model, Cmd.none)

-- View
view : Model -> Html Msg
view model =
    div [ class "container pt-4" ]
        [ div [ class "card border-danger shadow-sm" ]
            [ div [ class "card-header bg-danger text-white" ]
                [ h1 [ class "h5 mb-0" ] [ text ("Delete Semester #" ++ String.fromInt model.id) ] ]
            , div [ class "card-body" ]
                [ p []
                    [ text "Are you sure you want to delete this semester? This action cannot be undone." ]
                ]
            , div [ class "card-footer d-flex justify-content-end gap-2" ]
                [ button [ class "btn btn-secondary", onClick Cancel ] [ text "Cancel" ]
                , button [ class "btn btn-danger", onClick ConfirmDelete ] [ text "Delete" ]
                ]
            ]
        ]
