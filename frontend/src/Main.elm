module Main exposing (main)

import Browser
import Html exposing (text)
import View.Page exposing (view)

main =
    Browser.sandbox { init = (), view = \_ -> view, update = \_ model -> model }
