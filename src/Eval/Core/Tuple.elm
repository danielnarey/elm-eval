module Eval.Core.Tuple exposing
  ( lib )


-- Project
import Eval.Core.Error as Error
import Eval.Encode as Encode
import Eval.Function exposing (Function(..))
import Eval.Try as Try
import Eval.Wrap as Wrap

-- Core
import Json.Encode exposing (Value)


lib : String -> Result String Function
lib fName =
  case fName of
    "pair" ->
      Wrap.a2 Tuple.pair (Just, Just) (Encode.tuple2 (Encode.value, Encode.value)) (Error.expected fName "[any, any]")
        |> F2
        |> Ok

    "first" ->
      Wrap.a1 Tuple.first (Try.tuple2) Encode.value (Error.expected fName "[array-2]")
        |> F1
        |> Ok

    "second" ->
      Wrap.a1 Tuple.second (Try.tuple2) Encode.value (Error.expected fName "[array-2]")
        |> F1
        |> Ok

    "mapFirst" ->
      Err (Error.noFunction fName)

    "mapSecond" ->
      Err (Error.noFunction fName)

    "mapBoth" ->
      Err (Error.noFunction fName)

    _ ->
      Err (Error.notFound "Tuple" fName)
