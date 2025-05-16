module View.Layout.Header exposing (view)

import Html exposing (Html, a, button, div, nav, span, text, ul, li, header)
import Html.Attributes exposing (class, href, id, type_)

view : Html msg
view =
    header [ ]
        [ nav [ class "navbar navbar-expand-lg bg-primary" ]
        [ div [ class "container-fluid" ]
            [ a [ class "navbar-brand", href "#" ] [ text "Notentool" ]
            , button
                [ class "navbar-toggler"
                , type_ "button"
                ]
                [ span [ class "navbar-toggler-icon" ] [] ]
            , div [ class "collapse navbar-collapse", id "navbarNav" ]
                [ ul [ class "navbar-nav" ]
                    [ li [ class "nav-item" ]
                        [ a [ class "nav-link active", href "#" ]
                            [ text "Home" ]
                        ]
                    ]
                ]
            ]
        ] ]