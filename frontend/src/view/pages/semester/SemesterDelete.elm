module View.Pages.Semester.SemesterDelete exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Shared.Models.Semester exposing (Semester)
import Shared.Services.SemesterService as SemesterService

type Msg
    = ConfirmDelete
    | Cancel
    | GotSemester (Result Http.Error Semester)
    | SemesterDeleted (Result Http.Error ())

type alias Model =
    { id : Int
    , name : Maybe String
    , loading : Bool
    , error : Maybe String
    }

init : Int -> (Model, Cmd Msg)
init id =
    ( { id = id
      , name = Nothing
      , loading = True
      , error = Nothing
      }
    , SemesterService.getById id GotSemester
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ConfirmDelete ->
            ( { model | loading = True }
            , SemesterService.delete model.id SemesterDeleted
            )

        Cancel ->
            (model, Cmd.none)

        GotSemester (Ok semester) ->
            ( { model 
              | name = Just semester.name
              , loading = False
              }
            , Cmd.none
            )

        GotSemester (Err _) ->
            ( { model 
              | loading = False
              , error = Just "Failed to load semester"
              }
            , Cmd.none
            )

        SemesterDeleted (Ok _) ->
            update Cancel model

        SemesterDeleted (Err _) ->
            ( { model 
              | loading = False
              , error = Just "Failed to delete semester"
              }
            , Cmd.none
            )

-- View
view : Model -> Html Msg
view model =
    div [ class "container pt-4" ]
        [ div [ class "card border-danger shadow-sm" ]
            [ div [ class "card-header bg-danger text-white" ]
                [ h1 [ class "h5 mb-0" ] 
                    [ text ("Delete Semester " ++ 
                        (case model.name of
                            Just name -> name
                            Nothing -> "#" ++ String.fromInt model.id
                        ))
                    ] 
                ]
            , div [ class "card-body" ]
                [ if model.loading && model.name == Nothing then
                    div [] [ text "Loading..." ]
                  else if model.error /= Nothing then
                    div [ class "alert alert-danger" ] [ text (Maybe.withDefault "" model.error) ]
                  else
                    p []
                        [ text ("Are you sure you want to delete the semester \"" ++ 
                            (Maybe.withDefault ("ID: " ++ String.fromInt model.id) model.name) ++ 
                            "\"? This action cannot be undone.") 
                        ]
                ]
            , div [ class "card-footer d-flex justify-content-end gap-2" ]
                [ button [ class "btn btn-secondary", onClick Cancel ] [ text "Cancel" ]
                , button 
                    [ class "btn btn-danger"
                    , onClick ConfirmDelete
                    ] 
                    [ text "Delete" ]
                ]
            ]
        ]