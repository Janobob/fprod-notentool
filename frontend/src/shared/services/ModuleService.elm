module Shared.Services.ModuleService exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Shared.Models.Module exposing (Module, decoder, encoder)
import Shared.Models.Exam exposing (Exam)

baseUrl : String
baseUrl =
    "http://localhost:8080/api/modules"

getAll : (Result Http.Error (List Module) -> msg) -> Cmd msg
getAll toMsg =
    Http.get
        { url = baseUrl
        , expect = Http.expectJson toMsg (Decode.list decoder)
        }

getById : Int -> (Result Http.Error Module -> msg) -> Cmd msg
getById id toMsg =
    Http.get
        { url = baseUrl ++ "/" ++ String.fromInt id
        , expect = Http.expectJson toMsg decoder
        }

getExamsFromModuleId : Int -> (Result Http.Error (List Exam) -> msg) -> Cmd msg
getExamsFromModuleId moduleId toMsg =
    Http.get
        { url = baseUrl ++ "/" ++ String.fromInt moduleId ++ "/exams"
        , expect = Http.expectJson toMsg (Decode.list Shared.Models.Exam.decoder)
        }

create : Module -> (Result Http.Error Module -> msg) -> Cmd msg
create module_ toMsg =
    Http.post
        { url = baseUrl
        , body = Http.jsonBody (encoder module_)
        , expect = Http.expectJson toMsg decoder
        }

update : Int -> Module -> (Result Http.Error Module -> msg) -> Cmd msg
update id module_ toMsg =
    Http.request
        { method = "PUT"
        , url = baseUrl ++ "/" ++ String.fromInt id
        , body = Http.jsonBody (encoder module_)
        , expect = Http.expectJson toMsg decoder
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        }

delete : Int -> (Result Http.Error () -> msg) -> Cmd msg
delete id toMsg =
    Http.request
        { method = "DELETE"
        , url = baseUrl ++ "/" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        }