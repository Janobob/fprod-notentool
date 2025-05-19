module Shared.Models.Semester exposing (Semester, decoder)

import Json.Decode as Decode exposing (Decoder)

type alias Semester = 
    {
        id: Int,
        name: String
    }

decoder : Decode.Decoder Semester
decoder =
    Decode.map2 Semester
        (Decode.field "semesterId" Decode.int)
        (Decode.field "semesterName" Decode.string)