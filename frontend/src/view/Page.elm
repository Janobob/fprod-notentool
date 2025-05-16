module View.Page exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import View.Layout.Header as Header

view : Html msg
view =
    div [ class "d-flex flex-column min-vh-100" ]
        [ 
            Header.view
        ]