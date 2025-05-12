module Main exposing (main)

import Browser
import Html exposing (text)

main =
    Browser.sandbox { init = (), view = \_ -> text "Notentool!", update = \_ model -> model }
