module Eval.Core.Array exposing
  ( lib )


-- Project
import Eval.Function exposing (Function(..))
import Eval.Try as Try
import Eval.Wrap as Wrap
import Eval.Core.Error as Error

-- Core
import Array
import Json.Decode exposing (Value)
import Json.Encode as Encode


lib : String -> Result String Function
lib fName =
  case fName of
    "empty" ->
      Wrap.a0 (\() -> Array.empty) (Encode.array (\v -> v))
        |> F0
        |> Ok

    _ ->
      Err (Error.notFound "Array" fName)
