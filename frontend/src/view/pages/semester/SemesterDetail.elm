module View.Pages.Semester.SemesterDetail exposing (view)

import Html exposing (Html, div, h1, text)

view : Int -> Html msg
view id =
    div []
        [ h1 [] [ text ("Semester Detail: " ++ String.fromInt id) ] ]