module View.Pages.Semester.SemesterEdit exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, h1, input, label, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onInput, onClick)
import Http
import Shared.Models.Semester exposing (Semester)
import Shared.Services.SemesterService as SemesterService

type Msg
    = NameChanged String
    | Submit
    | Cancel
    | GotSemester (Result Http.Error Semester)
    | SemesterUpdated (Result Http.Error Semester)

type alias Model =
    { id : Int
    , name : String
    , loading : Bool
    , error : Maybe String
    , successMessage : Maybe String
    }

init : Int -> (Model, Cmd Msg)
init id =
    ( { id = id
      , name = ""
      , loading = True
      , error = Nothing
      , successMessage = Nothing
      }
    , SemesterService.getById id GotSemester
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NameChanged newName ->
            ( { model | name = newName, successMessage = Nothing }, Cmd.none )

        Submit ->
            ( { model | loading = True, error = Nothing, successMessage = Nothing }
            , SemesterService.update model.id 
                { id = model.id, name = model.name } 
                SemesterUpdated
            )

        Cancel ->
            (model, Cmd.none)  -- Die Navigation wird in Main.elm behandelt

        GotSemester (Ok semester) ->
            ( { model 
              | name = semester.name
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

        SemesterUpdated (Ok _) ->
            -- Nach erfolgreichem Update das Semester neu laden und Erfolgsmeldung anzeigen
            ( { model 
              | loading = True
              , successMessage = Just "Semester successfully updated"
              }
            , SemesterService.getById model.id GotSemester
            )

        SemesterUpdated (Err _) ->
            ( { model 
              | loading = False
              , error = Just "Failed to update semester"
              }
            , Cmd.none
            )

view : Model -> Html Msg
view model =
    div [ class "container pt-4" ]
        [ div [ class "card shadow-sm" ]
            [ div [ class "card-header" ]
                [ h1 [ class "h5 mb-0" ] [ text ("Edit Semester #" ++ String.fromInt model.id) ] ]
            , div [ class "card-body" ]
                [ if model.loading && model.name == "" then
                    div [] [ text "Loading..." ]
                  else
                    div []
                        [ if model.error /= Nothing then
                            div [ class "alert alert-danger mb-3" ] 
                                [ text (Maybe.withDefault "" model.error) ]
                          else
                            text ""
                        , if model.successMessage /= Nothing then
                            div [ class "alert alert-success mb-3" ] 
                                [ text (Maybe.withDefault "" model.successMessage) ]
                          else
                            text ""
                        , div [ class "mb-3" ]
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
                ]
            , div [ class "card-footer d-flex justify-content-end gap-2" ]
                [ button [ class "btn btn-secondary", onClick Cancel ] [ text "Cancel" ]
                , button 
                    [ class "btn btn-primary"
                    , onClick Submit
                    ] 
                    [ text "Save" ]
                ]
            ]
        ]