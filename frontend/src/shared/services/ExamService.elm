module Shared.Services.ExamService exposing (..)

import Http
import Json.Decode as Decode
import Shared.Models.Exam exposing (Exam, decoder)

baseUrl : String
baseUrl =
    "http://localhost:8080/api"

getExamsFromModuleId : Int -> (Result Http.Error (List Exam) -> msg) -> Cmd msg
getExamsFromModuleId moduleId msg =
    Http.get
        { url = baseUrl ++ "/modules/" ++ String.fromInt moduleId ++ "/exams"
        , expect = Http.expectJson msg (Decode.list decoder)
        }