module Eval.Core.Dict exposing
  ( lib )


-- Project
import Eval.Function exposing (Function(..))
import Eval.Try as Try
import Eval.Wrap as Wrap
import Eval.Core.Error as Error

-- Core
import Dict
import Json.Decode exposing (Value)
import Json.Encode as Encode


lib : String -> Result String Function
lib fName =
  case fName of
    "union" ->
      Wrap.a2 (Dict.union) (Try.dict, Try.dict) (Dict.toList >> Encode.object) (Error.expected fName "[object, object]")
        |> F2
        |> Ok

    "intersect" ->
      Wrap.a2 (Dict.intersect) (Try.dict, Try.dict) (Dict.toList >> Encode.object) (Error.expected fName "[object, object]")
        |> F2
        |> Ok

    "diff" ->
      Wrap.a2 (Dict.diff) (Try.dict, Try.dict) (Dict.toList >> Encode.object) (Error.expected fName "[object, object]")
        |> F2
        |> Ok

    _ ->
      Err (Error.notFound "Dict" fName)
