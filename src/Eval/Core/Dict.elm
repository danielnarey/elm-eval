module Eval.Core.Dict exposing
  ( lib )


-- Project
import Eval.Core.Error as Error
import Eval.Encode as Encode
import Eval.Function exposing (Function(..))
import Eval.Try as Try
import Eval.Wrap as Wrap

-- Core
import Dict
import Json.Encode exposing (Value)


lib : String -> Result String Function
lib fName =
  let
    getter decoder encoder valueType (a, b) =
      case (Try.string a, Try.dict b) of
        (Just key, Just dict) ->
          case (dict |> Dict.get key) of
            Just value ->
              case (decoder value) of
                Just decodedValue ->
                  Ok (encoder decodedValue)

                Nothing ->
                  Err (Error.expected fName ("[string, object(" ++ valueType ++ ")]"))

            Nothing ->
              Err (
                "The object passed to this function doesn't contain the key '"
                ++ key
                ++ "'."
              )

        (_, _) ->
          Err (Error.expected fName "[string, object]")

  in
    case fName of
      "empty" ->
        Wrap.a0 (\() -> Dict.empty) Encode.dict
          |> F0
          |> Ok

      "singleton" ->
        Wrap.a2 Dict.singleton (Try.string, Just) Encode.dict (Error.expected fName "[string, any]")
          |> F2
          |> Ok

      "insert" ->
        Wrap.a3 Dict.insert (Try.string, Just, Try.dict) Encode.dict (Error.expected fName "[string, any, object]")
          |> F3
          |> Ok

      "update" ->
        Err (Error.noFunction fName)

      "remove" ->
        Wrap.a2 Dict.remove (Try.string, Try.dict) Encode.dict (Error.expected fName "[string, object]")
          |> F2
          |> Ok

      "isEmpty" ->
        Wrap.a1 Dict.isEmpty Try.dict Encode.bool (Error.expected fName "[object]")
          |> F1
          |> Ok

      "member" ->
        Wrap.a2 Dict.member (Try.string, Try.dict) Encode.bool (Error.expected fName "[string, object]")
          |> F2
          |> Ok

      "get" ->
        getter Just Encode.value "any"
          |> F2
          |> Ok

      "get.string" ->
        getter Try.string Encode.string "string"
          |> F2
          |> Ok

      "get.char" ->
        getter Try.char Encode.char "string-1"
          |> F2
          |> Ok

      "get.int" ->
        getter Try.int Encode.int "integer"
          |> F2
          |> Ok

      "get.float" ->
        getter Try.float Encode.float "number"
          |> F2
          |> Ok

      "get.bool" ->
        getter Try.bool Encode.bool "boolean"
          |> F2
          |> Ok

      "get.list" ->
        getter Try.list Encode.list "array"
          |> F2
          |> Ok

      "get.array" ->
        getter Try.array Encode.array "array"
          |> F2
          |> Ok

      "get.dict" ->
        getter Try.dict Encode.dict "object"
          |> F2
          |> Ok

      "size" ->
        Wrap.a1 Dict.size Try.dict Encode.int (Error.expected fName "[object]")
          |> F1
          |> Ok

      "keys" ->
        Wrap.a1 Dict.keys Try.dict Encode.listString (Error.expected fName "[object]")
          |> F1
          |> Ok

      "values" ->
        Wrap.a1 Dict.values Try.dict Encode.list (Error.expected fName "[object]")
          |> F1
          |> Ok

      "toList" ->
        Wrap.a1 Dict.toList Try.dict (Encode.listTuple2 (Encode.string, Encode.value)) (Error.expected fName "[object]")
          |> F1
          |> Ok

      "fromList" ->
        Wrap.a1 Dict.fromList Try.listKeyValue Encode.dict (Error.expected fName "[array(array-2)]")
          |> F1
          |> Ok

      "map" ->
        Err (Error.noFunction fName)

      "foldl" ->
        Err (Error.noFunction fName)

      "foldr" ->
        Err (Error.noFunction fName)

      "filter" ->
        Err (Error.noFunction fName)

      "partition" ->
        Err (Error.noFunction fName)

      "union" ->
        Wrap.a2 Dict.union (Try.dict, Try.dict) Encode.dict (Error.expected fName "[object, object]")
          |> F2
          |> Ok

      "intersect" ->
        Wrap.a2 Dict.intersect (Try.dict, Try.dict) Encode.dict (Error.expected fName "[object, object]")
          |> F2
          |> Ok

      "diff" ->
        Wrap.a2 Dict.diff (Try.dict, Try.dict) Encode.dict (Error.expected fName "[object, object]")
          |> F2
          |> Ok

      "merge" ->
        Err (Error.noFunction fName)

      _ ->
        Err (Error.notFound "Dict" fName)
