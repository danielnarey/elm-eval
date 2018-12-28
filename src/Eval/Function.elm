module Eval.Function exposing
  ( Function(..) )


import Json.Encode exposing (Value)


{-| An Elm function that takes one, two, or three arguments
-}
type Function
  = F0 (() -> Result String Value)
  | F1 (Value -> Result String Value)
  | F2 ((Value, Value) -> Result String Value)
  | F3 ((Value, Value, Value) -> Result String Value)
