module View.Pages.Module.ModuleAdd exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, button, div, h1, input, label, text, select, option)
import Html.Attributes exposing (class, type_, value, disabled, selected)
import Html.Events exposing (onClick, onInput)
import Shared.Models.Module exposing (Module)
import Shared.Services.ModuleService as ModuleService
import Http

type Msg
    = NameChanged String
    | AbbreviationChanged String
    | Submit
    | SubmitResult (Result Http.Error Module)
    | Cancel

type alias Model =
    { name : String
    , abbreviation : String
    , semesterId : Int
    , isSubmitting : Bool
    , error : Maybe String
    }

init : Int -> Model
init semesterId =
    { name = ""
    , abbreviation = ""
    , semesterId = semesterId
    , isSubmitting = False
    , error = Nothing
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NameChanged newName ->
            ( { model | name = newName, error = Nothing }, Cmd.none )

        AbbreviationChanged newAbbreviation ->
            ( { model | abbreviation = newAbbreviation, error = Nothing }, Cmd.none )

        Submit ->
            if String.isEmpty model.name then
                ( { model | error = Just "Module name is required." }, Cmd.none )
            else if String.isEmpty model.abbreviation then
                ( { model | error = Just "Module abbreviation is required." }, Cmd.none )
            else
                let
                    newModule =
                        { id = 0
                        , name = model.name
                        , abbreviation = model.abbreviation
                        , semesterId = model.semesterId
                        }
                in
                ( { model | isSubmitting = True, error = Nothing }
                , ModuleService.create newModule SubmitResult
                )

        SubmitResult result ->
            case result of
                Ok _ ->
                    update Cancel model

                Err _ ->
                    ( { model | isSubmitting = False, error = Just "Failed to create module." }
                    , Cmd.none
                    )

        Cancel ->
            (model, Cmd.none)

view : Model -> Html Msg
view model =
    div [ class "container pt-4" ]
        [ div [ class "card shadow-sm" ]
            [ div [ class "card-header" ]
                [ h1 [ class "h5 mb-0" ] [ text "Add Module" ] ]
            , div [ class "card-body" ]
                [ div [ class "mb-3" ]
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
                    [ label [ class "form-label" ] [ text "Semester ID" ]
                    , input
                        [ class "form-control"
                        , type_ "text"
                        , value (String.fromInt model.semesterId)
                        , disabled True
                        ]
                        []
                    ]
                , case model.error of
                    Just msg ->
                        div [ class "alert alert-danger mt-2" ] [ text msg ]

                    Nothing ->
                        text ""
                ]
            , div [ class "card-footer d-flex justify-content-end gap-2" ]
                [ button [ class "btn btn-secondary", onClick Cancel ] [ text "Cancel" ]
                , button
                    [ class "btn btn-primary"
                    , onClick Submit
                    , disabled model.isSubmitting
                    ]
                    [ text (if model.isSubmitting then "Creating..." else "Create") ]
                ]
            ]
        ]