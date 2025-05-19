module Shared.Services.SemesterService exposing (..)

import Http
import Json.Decode as Decode
import Shared.Models.Semester exposing (Semester, decoder)

baseUrl : String
baseUrl =
    "http://localhost:8080/api/semesters"

getAll : (Result Http.Error (List Semester) -> msg) -> Cmd msg
getAll toMsg =
    Http.get
        { url = baseUrl
        , expect = Http.expectJson toMsg (Decode.list decoder)
        }