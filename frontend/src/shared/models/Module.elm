module Shared.Models.Module exposing (Module, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

type alias Module =
    { id : Int
    , name : String
    , abbreviation : String
    , semesterId : Int
    }

decoder : Decoder Module
decoder =
    Decode.map4 Module
        (Decode.field "module_id" Decode.int)
        (Decode.field "module_name" Decode.string)
        (Decode.field "module_abbrevation" Decode.string)
        (Decode.field "module_semesterId" Decode.int)

encoder : Module -> Value
encoder m =
    Encode.object
        [ ("moduleId", Encode.int m.id)
        , ("moduleName", Encode.string m.name)
        , ("moduleAbbrevation", Encode.string m.abbreviation)
        , ("moduleSemesterId", Encode.int m.semesterId)
        ]