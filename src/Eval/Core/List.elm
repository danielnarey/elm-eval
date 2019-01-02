module Eval.Core.List exposing
  ( lib )


-- Project
import Eval.Function exposing (Function(..))
import Eval.Try as Try
import Eval.Wrap as Wrap
import Eval.Core.Error as Error

-- Core
import Json.Decode exposing (Value)
import Json.Encode as Encode


lib : String -> Result String Function
lib fName =
  case fName of
    "singleton" ->
      (\a -> [a] |> Encode.list (\v -> v) |> Ok)
        |> F1
        |> Ok

    "repeat" ->
      Wrap.a2 List.repeat (Try.int, Just) (Encode.list (\v -> v)) (Error.expected fName "[integer, any]")
        |> F2
        |> Ok

    "range" ->
      Wrap.a2 List.range (Try.int, Try.int) (Encode.list Encode.int) (Error.expected fName "[integer, integer]")
        |> F2
        |> Ok

    "(::)" ->
      Wrap.a2 (::) (Just, Try.list) (Encode.list (\v -> v)) (Error.expected fName "[any, array]")
        |> F2
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

    "filterMap" ->
      Err (Error.noFunction fName)

    "length" ->
      Wrap.a1 List.length Try.list Encode.int (Error.expected fName "[array]")
        |> F1
        |> Ok

    "reverse" ->
      Wrap.a1 List.reverse Try.list (Encode.list (\v -> v)) (Error.expected fName "[array]")
        |> F1
        |> Ok

    "member" ->
      Err (Error.noCompare fName)

    "all" ->
      Err (Error.noCompare fName)

    "any" ->
      Err (Error.noCompare fName)

    "maximum" ->
      Err (Error.noCompare fName)

    "minimum" ->
      Err (Error.noCompare fName)

    "sum" ->
      Wrap.a1 List.sum Try.listFloat Encode.float (Error.expected fName "[array(number)]")
        |> F1
        |> Ok

    "product" ->
      Wrap.a1 List.product Try.listFloat Encode.float (Error.expected fName "[array(number)]")
        |> F1
        |> Ok

    "append" ->
      Wrap.a2 List.append (Try.list, Try.list) (Encode.list (\v -> v)) (Error.expected fName "[array, array]")
        |> F2
        |> Ok

    "concat" ->
      Wrap.a1 List.concat Try.listList (Encode.list (\v -> v)) (Error.expected fName "[array(array)]")
        |> F1
        |> Ok

    "concatMap" ->
      Err (Error.noFunction fName)

    "intersperse" ->
      Wrap.a2 List.intersperse (Just, Try.list) (Encode.list (\v -> v)) (Error.expected fName "[any, array]")
        |> F2
        |> Ok

    "map2" ->
      Err (Error.noFunction fName)

    "map3" ->
      Err (Error.noFunction fName)

    "map4" ->
      Err (Error.noFunction fName)

    "map5" ->
      Err (Error.noFunction fName)

    "sort" ->
      Err (Error.noCompare fName)

    "sortBy" ->
      Err (Error.noCompare fName)

    "sortWith" ->
      Err (Error.noCompare fName)

    "isEmpty" ->
      Wrap.a1 List.isEmpty Try.list Encode.bool (Error.expected fName "[array]")
        |> F1
        |> Ok

    "head" ->
      ( \value ->
        case (value |> Try.list |> Maybe.withDefault []) of
          [] ->
            Err "Can't return the first element of an empty array."

          first :: _ ->
            Ok first

      )
        |> F1
        |> Ok

    "tail" ->
      ( \value ->
        case (value |> Try.list |> Maybe.withDefault []) of
          [] ->
            Err "Can't partition an empty array."

          _ :: rest ->
            rest
              |> Encode.list (\v -> v)
              |> Ok

      )
        |> F1
        |> Ok

    "take" ->
      Wrap.a2 List.take (Try.int, Try.list) (Encode.list (\v -> v)) (Error.expected fName "[integer, array]")
        |> F2
        |> Ok

    "drop" ->
      Wrap.a2 List.drop (Try.int, Try.list) (Encode.list (\v -> v)) (Error.expected fName "[integer, array]")
        |> F2
        |> Ok

    "partition" ->
      Err (Error.noCompare fName)

    "unzip" ->
      Wrap.a1 List.unzip Try.listTuple2 (\(a,b) -> [a,b] |> Encode.list (Encode.list (\v -> v))) (Error.expected fName "[array(array-2n)]")
        |> F1
        |> Ok

    _ ->
      Err (Error.notFound "List" fName)
