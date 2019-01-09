module Eval.Core.Char exposing
  ( lib )


-- Project
import Eval.Core.Error as Error
import Eval.Encode as Encode
import Eval.Function exposing (Function(..))
import Eval.Try as Try
import Eval.Wrap as Wrap

-- Core
import Char
import Json.Encode exposing (Value)


lib : String -> Result String Function
lib fName =
  case fName of
    "isUpper" ->
      Wrap.a1 Char.isUpper Try.char Encode.bool (Error.expected fName "[string-1]")
        |> F1
        |> Ok

    "isLower" ->
      Wrap.a1 Char.isLower Try.char Encode.bool (Error.expected fName "[string-1]")
        |> F1
        |> Ok

    "isAlpha" ->
      Wrap.a1 Char.isAlpha Try.char Encode.bool (Error.expected fName "[string-1]")
        |> F1
        |> Ok

    "isAlphaNum" ->
      Wrap.a1 Char.isAlphaNum Try.char Encode.bool (Error.expected fName "[string-1]")
        |> F1
        |> Ok

    "isDigit" ->
      Wrap.a1 Char.isDigit Try.char Encode.bool (Error.expected fName "[string-1]")
        |> F1
        |> Ok

    "isOctDigit" ->
      Wrap.a1 Char.isOctDigit Try.char Encode.bool (Error.expected fName "[string-1]")
        |> F1
        |> Ok

    "isHexDigit" ->
      Wrap.a1 Char.isHexDigit Try.char Encode.bool (Error.expected fName "[string-1]")
        |> F1
        |> Ok

    "toUpper" ->
      Wrap.a1 Char.toUpper Try.char Encode.char (Error.expected fName "[string-1]")
        |> F1
        |> Ok

    "toLower" ->
      Wrap.a1 Char.toLower Try.char Encode.char (Error.expected fName "[string-1]")
        |> F1
        |> Ok

    "toLocaleUpper" ->
      Wrap.a1 Char.toLocaleUpper Try.char Encode.char (Error.expected fName "[string-1]")
        |> F1
        |> Ok

    "toLocaleLower" ->
      Wrap.a1 Char.toLocaleLower Try.char Encode.char (Error.expected fName "[string-1]")
        |> F1
        |> Ok

    "toCode" ->
      Wrap.a1 Char.toCode Try.char Encode.int (Error.expected fName "[string-1]")
        |> F1
        |> Ok

    "fromCode" ->
      Wrap.a1 Char.fromCode Try.int Encode.char (Error.expected fName "[integer]")
        |> F1
        |> Ok

    _ ->
      Err (Error.notFound "Char" fName)
