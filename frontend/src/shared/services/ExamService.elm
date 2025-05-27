module Shared.Services.ExamService exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Shared.Models.Exam exposing (Exam, decoder, encoder)

baseUrl : String
baseUrl =
    "http://localhost:8080/api"

getExamsFromModuleId : Int -> (Result Http.Error (List Exam) -> msg) -> Cmd msg
getExamsFromModuleId moduleId msg =
    Http.get
        { url = baseUrl ++ "/modules/" ++ String.fromInt moduleId ++ "/exams"
        , expect = Http.expectJson msg (Decode.list decoder)
        }

getById : Int -> (Result Http.Error Exam -> msg) -> Cmd msg
getById id toMsg =
    Http.get
        { url = baseUrl ++ "/exams/" ++ String.fromInt id
        , expect = Http.expectJson toMsg decoder
        }

create : Int -> Exam -> (Result Http.Error Exam -> msg) -> Cmd msg
create moduleId exam toMsg =
    Http.post
        { url = baseUrl ++ "/exams"
        , body = Http.jsonBody (encoder exam)
        , expect = Http.expectJson toMsg decoder
        }

update : Int -> Exam -> (Result Http.Error Exam -> msg) -> Cmd msg
update id exam toMsg =
    Http.request
        { method = "PUT"
        , url = baseUrl ++ "/exams/" ++ String.fromInt id
        , body = Http.jsonBody (encoder exam)
        , expect = Http.expectJson toMsg decoder
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        }

delete : Int -> (Result Http.Error () -> msg) -> Cmd msg
delete id toMsg =
    Http.request
        { method = "DELETE"
        , url = baseUrl ++ "/exams/" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        }