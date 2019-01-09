module Eval.Core.String exposing
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
    "isEmpty" ->
      Wrap.a1 String.isEmpty Try.string Encode.bool (Error.expected fName "[string]")
        |> F1
        |> Ok

    "length" ->
      Wrap.a1 String.length Try.string Encode.int (Error.expected fName "[string]")
        |> F1
        |> Ok

    "reverse" ->
      Wrap.a1 String.reverse Try.string Encode.string (Error.expected fName "[string]")
        |> F1
        |> Ok

    "repeat" ->
      Wrap.a2 String.repeat (Try.int, Try.string) Encode.string (Error.expected fName "[integer, string]")
        |> F2
        |> Ok

    "replace" ->
      Wrap.a3 String.replace (Try.string, Try.string, Try.string) Encode.string (Error.expected fName "[string, string, string]")
        |> F3
        |> Ok

    "append" ->
      Wrap.a2 String.append (Try.string, Try.string) Encode.string (Error.expected fName "[string, string]")
        |> F2
        |> Ok

    "concat" ->
      Wrap.a1 String.concat Try.listString Encode.string (Error.expected fName "[array(string)]")
        |> F1
        |> Ok

    "split" ->
      Wrap.a2 String.split (Try.string, Try.string) Encode.listString (Error.expected fName "[string, string]")
        |> F2
        |> Ok

    "join" ->
      Wrap.a2 String.join (Try.string, Try.listString) Encode.string (Error.expected fName "[string, array(string)]")
        |> F2
        |> Ok

    "words" ->
      Wrap.a1 String.words Try.string Encode.listString (Error.expected fName "[string]")
        |> F1
        |> Ok

    "lines" ->
      Wrap.a1 String.lines Try.string Encode.listString (Error.expected fName "[string]")
        |> F1
        |> Ok

    "slice" ->
      Wrap.a3 String.slice (Try.int, Try.int, Try.string) Encode.string (Error.expected fName "[integer, integer, string]")
        |> F3
        |> Ok

    "left" ->
      Wrap.a2 String.left (Try.int, Try.string) Encode.string (Error.expected fName "[integer, string]")
        |> F2
        |> Ok

    "right" ->
      Wrap.a2 String.right (Try.int, Try.string) Encode.string (Error.expected fName "[integer, string]")
        |> F2
        |> Ok

    "dropLeft" ->
      Wrap.a2 String.dropLeft (Try.int, Try.string) Encode.string (Error.expected fName "[integer, string]")
        |> F2
        |> Ok

    "dropRight" ->
      Wrap.a2 String.dropRight (Try.int, Try.string) Encode.string (Error.expected fName "[integer, string]")
        |> F2
        |> Ok

    "contains" ->
      Wrap.a2 String.contains (Try.string, Try.string) Encode.bool (Error.expected fName "[string, string]")
        |> F2
        |> Ok

    "startsWith" ->
      Wrap.a2 String.startsWith (Try.string, Try.string) Encode.bool (Error.expected fName "[string, string]")
        |> F2
        |> Ok

    "endsWith" ->
      Wrap.a2 String.endsWith (Try.string, Try.string) Encode.bool (Error.expected fName "[string, string]")
        |> F2
        |> Ok

    "indexes" ->
      Wrap.a2 String.indexes (Try.string, Try.string) Encode.listInt (Error.expected fName "[string, string]")
        |> F2
        |> Ok

    "indices" ->
      Wrap.a2 String.indices (Try.string, Try.string) Encode.listInt (Error.expected fName "[string, string]")
        |> F2
        |> Ok

    "toInt" ->
      ( \value ->
        case (value |> Try.string) of
          Just string ->
            case (string |> String.toInt) of
              Just integer ->
                Ok (Encode.int integer)

              Nothing ->
                Err "Failed to parse the string argument as an integer value."

          Nothing ->
            Err (Error.expected fName "[string]")

      )
        |> F1
        |> Ok

    "fromInt" ->
      Wrap.a1 String.fromInt Try.int Encode.string (Error.expected fName "[integer]")
        |> F1
        |> Ok

    "toFloat" ->
      ( \value ->
        case (value |> Try.string) of
          Just string ->
            case (string |> String.toFloat) of
              Just number ->
                Ok (Encode.float number)

              Nothing ->
                Err "Failed to parse the string argument as an numeric value."

          Nothing ->
            Err (Error.expected fName "[string]")

      )
        |> F1
        |> Ok

    "fromFloat" ->
      Wrap.a1 String.fromFloat Try.float Encode.string (Error.expected fName "[number]")
        |> F1
        |> Ok

    "fromChar" ->
      Wrap.a1 String.fromChar Try.char Encode.string (Error.expected fName "[string-1]")
        |> F1
        |> Ok

    "cons" ->
      Wrap.a2 String.cons (Try.char, Try.string) Encode.string (Error.expected fName "[string-1, string]")
        |> F2
        |> Ok

    "uncons" ->
      ( \value ->
        case (value |> Try.string) of
          Just string ->
            case (string |> String.uncons) of
              Just tuple ->
                Ok (tuple |> Encode.tuple2 (Encode.char, Encode.string))

              Nothing ->
                Err "Can't partition an empty string."

          Nothing ->
            Err (Error.expected fName "[string]")

      )
        |> F1
        |> Ok

    "toList" ->
      Wrap.a1 String.toList Try.string Encode.listChar (Error.expected fName "[string]")
        |> F1
        |> Ok

    "fromList" ->
      Wrap.a1 String.fromList Try.listChar Encode.string (Error.expected fName "[array(string-1)]")
        |> F1
        |> Ok

    "toUpper" ->
      Wrap.a1 String.toUpper Try.string Encode.string (Error.expected fName "[string]")
        |> F1
        |> Ok

    "toLower" ->
      Wrap.a1 String.toLower Try.string Encode.string (Error.expected fName "[string]")
        |> F1
        |> Ok

    "pad" ->
      Wrap.a3 String.pad (Try.int, Try.char, Try.string) Encode.string (Error.expected fName "[integer, string-1, string]")
        |> F3
        |> Ok

    "padLeft" ->
      Wrap.a3 String.padLeft (Try.int, Try.char, Try.string) Encode.string (Error.expected fName "[integer, string-1, string]")
        |> F3
        |> Ok

    "padRight" ->
      Wrap.a3 String.padRight (Try.int, Try.char, Try.string) Encode.string (Error.expected fName "[integer, string-1, string]")
        |> F3
        |> Ok

    "trim" ->
      Wrap.a1 String.trim Try.string Encode.string (Error.expected fName "[string]")
        |> F1
        |> Ok

    "trimLeft" ->
      Wrap.a1 String.trimLeft Try.string Encode.string (Error.expected fName "[string]")
        |> F1
        |> Ok

    "trimRight" ->
      Wrap.a1 String.trimRight Try.string Encode.string (Error.expected fName "[string]")
        |> F1
        |> Ok

    "map" ->
      Err (Error.noFunction fName)

    "filter" ->
      Err (Error.noFunction fName)

    "foldl" ->
      Err (Error.noFunction fName)

    "foldr" ->
      Err (Error.noFunction fName)

    "any" ->
      Err (Error.noFunction fName)

    "all" ->
      Err (Error.noFunction fName)

    _ ->
      Err (Error.notFound "String" fName)
