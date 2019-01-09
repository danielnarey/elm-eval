module Eval.Core.Bitwise exposing
  ( lib )


-- Project
import Eval.Core.Error as Error
import Eval.Encode as Encode
import Eval.Function exposing (Function(..))
import Eval.Try as Try
import Eval.Wrap as Wrap

-- Core
import Bitwise
import Json.Encode exposing (Value)


lib : String -> Result String Function
lib fName =
  case fName of
    "and" ->
      Wrap.a2 Bitwise.and (Try.int, Try.int) Encode.int (Error.expected fName "[integer, integer]")
        |> F2
        |> Ok

    "or" ->
      Wrap.a2 Bitwise.or (Try.int, Try.int) Encode.int (Error.expected fName "[integer, integer]")
        |> F2
        |> Ok

    "xor" ->
      Wrap.a2 Bitwise.xor (Try.int, Try.int) Encode.int (Error.expected fName "[integer, integer]")
        |> F2
        |> Ok

    "complement" ->
      Wrap.a1 Bitwise.complement Try.int Encode.int (Error.expected fName "[integer]")
        |> F1
        |> Ok

    "shiftLeftBy" ->
      Wrap.a2 Bitwise.shiftLeftBy (Try.int, Try.int) Encode.int (Error.expected fName "[integer, integer]")
        |> F2
        |> Ok

    "shiftRightBy" ->
      Wrap.a2 Bitwise.shiftRightBy (Try.int, Try.int) Encode.int (Error.expected fName "[integer, integer]")
        |> F2
        |> Ok

    "shiftRightZfBy" ->
      Wrap.a2 Bitwise.shiftRightBy (Try.int, Try.int) Encode.int (Error.expected fName "[integer, integer]")
        |> F2
        |> Ok

    _ ->
      Err (Error.notFound "Bitwise" fName)
