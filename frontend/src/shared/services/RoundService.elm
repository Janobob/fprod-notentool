module Shared.Services.RoundService exposing (round)

round : Int -> Float -> String
round precision number =
  let
    factor = toFloat (10 ^ precision)
    rounded = toFloat (roundHelper (number * factor)) / factor
  in
  String.fromFloat rounded

roundHelper : Float -> Int
roundHelper number =
  if number >= 0 then
    floor (number + 0.5)
  else
    ceiling (number - 0.5)