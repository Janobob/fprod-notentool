module Shared.Services.ModuleService exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Shared.Models.Module exposing (Module, decoder, encoder)
baseUrl : String
baseUrl =
    "http://localhost:8080/api/modules"
