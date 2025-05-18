module View.Pages.Semester.SemesterDelete exposing (view)

import Html exposing (Html, div, h1, text)

view : Int -> Html msg
view id =
    div []
        [ h1 [] [ text ("Semester Delete: " ++ String.fromInt id) ] ]