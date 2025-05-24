module Shared.Models.Exam exposing (Exam, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

type alias Exam =
    { id : Int
    , name : String
    , grade : Float
    , weight : Float
    , moduleId : Int
    }

decoder : Decoder Exam
decoder =
    Decode.map5 Exam
        (Decode.field "exam_id" Decode.int)
        (Decode.field "exam_name" Decode.string)
        (Decode.field "exam_grade" Decode.float)
        (Decode.field "exam_weight" Decode.float)
        (Decode.field "exam_moduleId" Decode.int)

encoder : Exam -> Value
encoder exam =
    Encode.object
        [ ("exam_id", Encode.int exam.id)
        , ("exam_name", Encode.string exam.name)
        , ("exam_grade", Encode.float exam.grade)
        , ("exam_weight", Encode.float exam.weight)
        , ("exam_moduleId", Encode.int exam.moduleId)
        ]