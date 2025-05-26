module View.Pages.Module.ModuleEdit exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, h1, input, label, text, select, option)
import Html.Attributes exposing (class, type_, value, selected)
import Html.Events exposing (onInput, onClick)
import Http
import Shared.Models.Module exposing (Module)
import Shared.Models.Semester exposing (Semester)
import Shared.Services.ModuleService as ModuleService
import Shared.Services.SemesterService as SemesterService

type Msg
    = NameChanged String
    | AbbreviationChanged String
    | SemesterIdChanged String
    | Submit
    | Cancel
    | GotModule (Result Http.Error Module)
    | GotSemesters (Result Http.Error (List Semester))
    | ModuleUpdated (Result Http.Error Module)

type alias Model =
    { id : Int
    , name : String
    , abbreviation : String
    , semesterId : Int
    , semesters : List Semester
    , loading : Bool
    , error : Maybe String
    , successMessage : Maybe String
    }

init : Int -> (Model, Cmd Msg)
init id =
    ( { id = id
      , name = ""
      , abbreviation = ""
      , semesterId = 0
      , semesters = []
      , loading = True
      , error = Nothing
      , successMessage = Nothing
      }
    , Cmd.batch
        [ ModuleService.getById id GotModule
        , SemesterService.getAll GotSemesters
        ]
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NameChanged newName ->
            ( { model | name = newName, successMessage = Nothing }, Cmd.none )

        AbbreviationChanged newAbbreviation ->
            ( { model | abbreviation = newAbbreviation, successMessage = Nothing }, Cmd.none )

        SemesterIdChanged idStr ->
            case String.toInt idStr of
                Just id ->
                    ( { model | semesterId = id, successMessage = Nothing }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )

        Submit ->
            ( { model | loading = True, error = Nothing, successMessage = Nothing }
            , ModuleService.update model.id 
                { id = model.id
                , name = model.name
                , abbreviation = model.abbreviation
                , semesterId = model.semesterId
                } 
                ModuleUpdated
            )

        Cancel ->
            (model, Cmd.none)

        GotModule (Ok module_) ->
            ( { model 
              | name = module_.name
              , abbreviation = module_.abbreviation
              , semesterId = module_.semesterId
              , loading = False
              }
            , Cmd.none
            )

        GotModule (Err _) ->
            ( { model 
              | loading = False
              , error = Just "Failed to load module"
              }
            , Cmd.none
            )

        GotSemesters (Ok semesters) ->
            ( { model | semesters = semesters }, Cmd.none )

        GotSemesters (Err _) ->
            ( { model | error = Just "Failed to load semesters" }, Cmd.none )

        ModuleUpdated (Ok _) ->
            ( { model 
              | loading = True
              , successMessage = Just "Module successfully updated"
              }
            , ModuleService.getById model.id GotModule
            )

        ModuleUpdated (Err _) ->
            ( { model 
              | loading = False
              , error = Just "Failed to update module"
              }
            , Cmd.none
            )

view : Model -> Html Msg
view model =
    div [ class "container pt-4" ]
        [ div [ class "card shadow-sm" ]
            [ div [ class "card-header" ]
                [ h1 [ class "h5 mb-0" ] [ text ("Edit Module #" ++ String.fromInt model.id) ] ]
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
                            [ label [ class "form-label" ] [ text "Module Name" ]
                            , input
                                [ class "form-control"
                                , type_ "text"
                                , value model.name
                                , onInput NameChanged
                                ]
                                []
                            ]
                        , div [ class "mb-3" ]
                            [ label [ class "form-label" ] [ text "Abbreviation" ]
                            , input
                                [ class "form-control"
                                , type_ "text"
                                , value model.abbreviation
                                , onInput AbbreviationChanged
                                ]
                                []
                            ]
                        , div [ class "mb-3" ]
                            [ label [ class "form-label" ] [ text "Semester" ]
                            , select 
                                [ class "form-select"
                                , onInput SemesterIdChanged
                                ] 
                                (option [ value "0", selected (model.semesterId == 0) ] [ text "-- Select Semester --" ] ::
                                    List.map 
                                        (\s -> option 
                                            [ value (String.fromInt s.id)
                                            , selected (model.semesterId == s.id)
                                            ] 
                                            [ text s.name ]
                                        ) 
                                        model.semesters
                                )
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