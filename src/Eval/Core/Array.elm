module Eval.Core.Array exposing
  ( lib )


-- Project
import Eval.Core.Error as Error
import Eval.Encode as Encode
import Eval.Function exposing (Function(..))
import Eval.Try as Try
import Eval.Wrap as Wrap

-- Core
import Array
import Json.Encode exposing (Value)


lib : String -> Result String Function
lib fName =
  let
    getter db encoder tb (a, b) =
      case (Try.int a, db b) of
        (Just index, Just array) ->
          case (array |> Array.get index) of
            Just value ->
              Ok (encoder value)

            Nothing ->
              Err (
                "Can't get the element at index "
                ++ (index |> String.fromInt)
                ++ " from an array of length "
                ++ (array |> Array.length |> String.fromInt)
                ++ "."
              )

        (_, _) ->
          Err (Error.expected fName ("[int, " ++ tb ++ "]"))

    setter (db, dc) encoder (tb, tc) (a, b, c) =
      case (Try.int a, db b, dc c) of
        (Just index, Just value, Just array) ->
          case (array |> Array.get index) of
            Just _ ->
              array
                |> Array.set index value
                |> encoder
                |> Ok

            Nothing ->
              Err (
                "Can't set the element at index "
                ++ (index |> String.fromInt)
                ++ " on an array of length "
                ++ (array |> Array.length |> String.fromInt)
                ++ "."
              )

        (_, _, _) ->
          Err (Error.expected fName ("[int, " ++ tb ++ ", " ++ tc ++ "]"))


  in
  case fName of
    "empty" ->
      Wrap.a0 (\() -> Array.empty) Encode.array
        |> F0
        |> Ok

    "initialize" ->
      Err (Error.noFunction fName)

    "repeat" ->
      Wrap.a2 Array.repeat (Try.int, Just) Encode.array (Error.expected fName "[integer, any]")
        |> F2
        |> Ok

    "fromList" ->
      Wrap.a1 Array.fromList Try.list Encode.array (Error.expected fName "[array]")
        |> F1
        |> Ok

    "isEmpty" ->
      Wrap.a1 Array.isEmpty Try.array Encode.bool (Error.expected fName "[array]")
        |> F1
        |> Ok

    "length" ->
      Wrap.a1 Array.length Try.array Encode.int (Error.expected fName "[array]")
        |> F1
        |> Ok

    "get" ->
      getter Try.array Encode.value "array"
        |> F2
        |> Ok

    "get.string" ->
      getter Try.arrayString Encode.string "array(string)"
        |> F2
        |> Ok

    "get.char" ->
      getter Try.arrayChar Encode.char "array(char)"
        |> F2
        |> Ok

    "get.int" ->
      getter Try.arrayInt Encode.int "array(int)"
        |> F2
        |> Ok

    "get.float" ->
      getter Try.arrayFloat Encode.float "array(float)"
        |> F2
        |> Ok

    "set" ->
      setter (Just, Try.array) Encode.array ("value", "array")
        |> F3
        |> Ok

    "set.string" ->
      setter (Try.string, Try.arrayString) Encode.arrayString ("string", "array(string)")
        |> F3
        |> Ok

    "set.char" ->
      setter (Try.char, Try.arrayChar) Encode.arrayChar ("string-1", "array(string-1)")
        |> F3
        |> Ok

    "set.int" ->
      setter (Try.int, Try.arrayInt) Encode.arrayInt ("integer", "array(integer)")
        |> F3
        |> Ok

    "set.float" ->
      setter (Try.float, Try.arrayFloat) Encode.arrayFloat ("number", "array(number)")
        |> F3
        |> Ok

    "push" ->
      Wrap.a2 Array.push (Just, Try.array) Encode.array (Error.expected fName "[any, array]")
        |> F2
        |> Ok

    "push.string" ->
      Wrap.a2 Array.push (Try.string, Try.arrayString) Encode.arrayString (Error.expected fName "[string, array(string)]")
        |> F2
        |> Ok

    "push.char" ->
      Wrap.a2 Array.push (Try.char, Try.arrayChar) Encode.arrayChar (Error.expected fName "[string-1, array(string-1)]")
        |> F2
        |> Ok

    "push.int" ->
      Wrap.a2 Array.push (Try.int, Try.arrayInt) Encode.arrayInt (Error.expected fName "[integer, array(integer)]")
        |> F2
        |> Ok

    "push.float" ->
      Wrap.a2 Array.push (Try.float, Try.arrayFloat) Encode.arrayFloat (Error.expected fName "[float, array(float)]")
        |> F2
        |> Ok

    "append" ->
      Wrap.a2 Array.append (Try.array, Try.array) Encode.array (Error.expected fName "[array, array]")
        |> F2
        |> Ok

    "append.string" ->
      Wrap.a2 Array.append (Try.arrayString, Try.arrayString) Encode.arrayString (Error.expected fName "[array(string), array(string)]")
        |> F2
        |> Ok

    "append.char" ->
      Wrap.a2 Array.append (Try.arrayChar, Try.arrayChar) Encode.arrayChar (Error.expected fName "[array(string-1), array(string-1)]")
        |> F2
        |> Ok

    "append.int" ->
      Wrap.a2 Array.append (Try.arrayInt, Try.arrayInt) Encode.arrayInt (Error.expected fName "[array(integer), array(integer)]")
        |> F2
        |> Ok

    "append.float" ->
      Wrap.a2 Array.append (Try.arrayFloat, Try.arrayFloat) Encode.arrayFloat (Error.expected fName "[array(number), array(number)]")
        |> F2
        |> Ok

    "slice" ->
      Wrap.a3 Array.slice (Try.int, Try.int, Try.array) Encode.array (Error.expected fName "[integer, integer, array]")
        |> F3
        |> Ok

    "toList" ->
      Wrap.a1 Array.toList Try.array Encode.list (Error.expected fName "[array]")
        |> F1
        |> Ok

    "toIndexedList" ->
      Wrap.a1 Array.toIndexedList Try.array (Encode.listTuple2 (Encode.int, Encode.value)) (Error.expected fName "[array]")
        |> F1
        |> Ok

    "map" ->
      Err (Error.noFunction fName)

    "indexedMap" ->
      Err (Error.noFunction fName)

    "foldl" ->
      Err (Error.noFunction fName)

    "foldr" ->
      Err (Error.noFunction fName)

    "filter" ->
      Err (Error.noFunction fName)

    _ ->
      Err (Error.notFound "Array" fName)
