module View.Pages.Semester.SemesterEdit exposing (view)

import Html exposing (Html, div, h1, text)

view : Int -> Html msg
view id =
    div []
        [ h1 [] [ text ("Semester Edit: " ++ String.fromInt id) ] ]