module View.Pages.Semester.SemesterDetail exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, h1, text, ul, li, p, h5)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Shared.Models.Module exposing (Module)
import Shared.Services.SemesterService as SemesterService

type alias Model =
    { semesterId : Int
    , modules : List Module
    , loading : Bool
    , error : Maybe String
    }

type Msg
    = ModulesLoaded (Result Http.Error (List Module))
    | NavigateToModuleDetail Int
    | EditModule Int
    | DeleteModule Int

init : Int -> (Model, Cmd Msg)
init semesterId =
    ( { semesterId = semesterId, modules = [], loading = True, error = Nothing }
    , SemesterService.getModulesFromSemesterId semesterId ModulesLoaded
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ModulesLoaded result ->
            case result of
                Ok modules ->
                    ( { model | modules = modules, loading = False }, Cmd.none )

                Err _ ->
                    ( { model | loading = False, error = Just "Failed to load modules." }, Cmd.none )

        NavigateToModuleDetail id ->
            (model, Cmd.none)
    
        EditModule id ->
            (model, Cmd.none)

        DeleteModule id ->
            (model, Cmd.none)

view : Model -> Html Msg
view model =
    div [ class "container pt-4" ]
        ([ h1 [ class "mb-4" ] [ text ("Semester Detail: " ++ String.fromInt model.semesterId) ] ]
            ++ (if model.loading then
                    [ div [ class "text-center" ] [ text "Loading modules..." ] ]

                else
                    case model.error of
                        Just msg ->
                            [ div [ class "alert alert-danger" ] [ text msg ] ]

                        Nothing ->
                            [ div [ class "row g-3" ]
                                (List.map moduleCard model.modules)
                            ]
               )
        )

moduleCard : Module -> Html Msg
moduleCard module_ =
    div [ class "col-md-4" ]
        [ div [ class "card module-card", onClick (NavigateToModuleDetail module_.id) ]
            [ div [ class "card-body position-relative" ]
                [ h5 [ class "card-title" ] [ text (module_.abbreviation ++ ": " ++ module_.name) ]
                , p [ class "card-text" ] [ text ("Module ID: " ++ String.fromInt module_.id) ]
                , div [ class "position-absolute top-0 end-0 p-2 d-hover-flex gap-2" ]
                    [ button
                        [ class "btn btn-sm btn-outline-secondary"
                        , onClick (EditModule module_.id)
                        ]
                        [ text "Edit" ]
                    , button
                        [ class "btn btn-sm btn-outline-danger"
                        , onClick (DeleteModule module_.id)
                        ]
                        [ text "Delete" ]
                    ]
                ]
            ]
        ]
