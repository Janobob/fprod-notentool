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
        (Decode.field "semesterId" Decode.int)
        (Decode.field "semesterName" Decode.string)

encoder : Semester -> Value
encoder semester =
    Encode.object
        [ ("name", Encode.string semester.name) ]