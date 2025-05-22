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
        [ ("module_id", Encode.int m.id)
        , ("module_name", Encode.string m.name)
        , ("module_abbrevation", Encode.string m.abbreviation)
        , ("module_semesterId", Encode.int m.semesterId)
        ]