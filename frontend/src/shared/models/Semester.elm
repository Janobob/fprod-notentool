module Shared.Models.Semester exposing (Semester, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

type alias Semester = 
    {
        id: Int,
        name: String
    }

decoder : Decoder Semester
decoder =
    Decode.map2 Semester
        (Decode.field "semester_id" Decode.int)
        (Decode.field "semester_name" Decode.string)

encoder : Semester -> Value
encoder semester =
    Encode.object
        [ ("semesterName", Encode.string semester.name) ]