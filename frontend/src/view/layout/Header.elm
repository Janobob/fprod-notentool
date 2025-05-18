module View.Layout.Header exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, a, button, div, nav, span, text, ul, li, header)
import Html.Attributes exposing (class, href, id, type_)
import Html.Events exposing (onClick)

type alias Model =
    {
        isCollapsed : Bool
    }

init : Model
init =
    { isCollapsed = True }

type Msg
    = ToggleCollapse

update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleCollapse ->
            { model | isCollapsed = not model.isCollapsed }

view : Model -> Html Msg
view model =
    header [ ]
        [ nav [ class "navbar navbar-expand-lg bg-primary" ]
        [ div [ class "container-fluid" ]
            [ a [ class "navbar-brand", href "/" ] [ text "Notentool" ]
            , button
                [ class "navbar-toggler"
                , type_ "button"
                , onClick ToggleCollapse
                ]
                [ span [ class "navbar-toggler-icon" ] [] ]
            ,  div
                    [ class
                        ("collapse navbar-collapse"
                            ++ (if model.isCollapsed then "" else " show")
                        )
                    , id "navbarNav" ]
                [ ul [ class "navbar-nav" ]
                    [ li [ class "nav-item" ]
                        -- todo: add route with msg
                        [ a [ class "nav-link active", href "/" ]
                            [ text "Home" ]
                        ]
                    ]
                ]
            ]
        ] ]