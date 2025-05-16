module Main exposing (main)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import View.Layout.Header as Header
import View.Pages.Semester.SemesterList as SemesterList

type Page =
    SemesterList SemesterList.Model

type alias Model =
    { page : Page }

type Msg = 
    SemesterListMsg SemesterList.Msg

init : () -> (Model, Cmd Msg)
init _ =
    ({ page = SemesterList SemesterList.init }, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (model.page, msg) of
        (SemesterList smodel, SemesterListMsg smsg) ->
            let
                (updatedModel, cmd) =
                    SemesterList.update smsg smodel
            in
            ( { model | page = SemesterList updatedModel }, Cmd.map SemesterListMsg cmd )

view : Model -> Html Msg
view model =
    div [ class "d-flex flex-column min-vh-100" ]
        [ 
            Header.view
        , div [ class "container" ]
            [ case model.page of
                SemesterList smodel ->
                    Html.map SemesterListMsg (SemesterList.view smodel)
            ]
        ]

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
