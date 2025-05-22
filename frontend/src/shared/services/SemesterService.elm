module Shared.Services.SemesterService exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Shared.Models.Semester as Semester exposing (Semester, decoder, encoder)
import Shared.Models.Module as Module exposing (Module, decoder, encoder)

baseUrl : String
baseUrl =
    "http://localhost:8080/api/semesters"

getAll : (Result Http.Error (List Semester) -> msg) -> Cmd msg
getAll toMsg =
    Http.get
        { url = baseUrl
        , expect = Http.expectJson toMsg (Decode.list Semester.decoder)
        }

create : Semester -> (Result Http.Error Semester -> msg) -> Cmd msg
create semester toMsg =
    Http.post
        { url = baseUrl
        , body = Http.jsonBody (Semester.encoder semester)
        , expect = Http.expectJson toMsg Semester.decoder
        }

getModulesFromSemesterId : Int -> (Result Http.Error (List Module) -> msg) -> Cmd msg
getModulesFromSemesterId semesterId toMsg =
    Http.get
        { url = baseUrl ++ "/" ++ String.fromInt semesterId ++ "/modules"
        , expect = Http.expectJson toMsg (Decode.list Module.decoder)
        }