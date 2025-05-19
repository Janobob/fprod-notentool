module Shared.Services.SemesterService exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Shared.Models.Semester exposing (Semester, decoder, encoder)

baseUrl : String
baseUrl =
    "http://localhost:8080/api/semesters"

getAll : (Result Http.Error (List Semester) -> msg) -> Cmd msg
getAll toMsg =
    Http.get
        { url = baseUrl
        , expect = Http.expectJson toMsg (Decode.list decoder)
        }

create : Semester -> (Result Http.Error Semester -> msg) -> Cmd msg
create semester toMsg =
    Http.post
        { url = baseUrl
        , body = Http.jsonBody (encoder semester)
        , expect = Http.expectJson toMsg decoder
        }