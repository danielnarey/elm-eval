module Eval.Core exposing
  ( lib )


-- Project
import Eval.Function exposing (Function(..))
import Eval.Try as Try
import Eval.Wrap as Wrap
-- Core
import Dict exposing (Dict)
import Json.Decode exposing (Value)
import Json.Encode as Encode


{-| Given the name of a function in the Elm Core library, return an equivalent
function that takes one or more `Json` values as arguments (either as a single
value, 2-tuple, or 3-tuple).
-}
lib : String -> Result String Function
lib expression =
  let
    parts =
      expression
        |> String.split "."

    (moduleName, fName) =
      case (parts |> List.length, parts, parts |> List.drop 1) of
        (1, first :: rest, []) ->
          ("Basics", first)
        (2, first :: rest, second :: []) ->
          (first, second)
        (_, _, _) ->
          ("", "")

    notFoundError =
      "A module named `"
      ++ moduleName
      ++ "` was not found in Elm's core libraries. "
      ++ "Note that Elm module names are always capitalized and that "
      ++ "module and function names should be separated by a single `.` "
      ++ "(example: `String.length`)."

  in
    case moduleName of
      "Basics" ->
        basics fName
      "Dict" ->
        dict fName
      _ ->
        Err notFoundError


--- MODULES ---

basics : String -> Result String Function
basics fName =
  let
    notFoundError =
      "Function `"
      ++ fName
      ++ "` was not found in the `Basics` core library. "
      ++ "If you are trying to access a function in another core library, "
      ++ "the module name must be given first (example: `String.length`)."

    noCompareError =
      "Comparison functions like `(==)` or `(>)` can't be called throught this "
      ++ "interface because Elm doesn't support type inference from JavaScript "
      ++ "values."

    typeError typeList =
      "Type error in arguments to `"
      ++ fName
      ++ "`: expected "
      ++ typeList
      ++ "."

  in
    case fName of
      "(+)" ->
        Wrap.a2 (+) (Try.float, Try.float) Encode.float (typeError "[number, number]")
          |> F2
          |> Ok

      "(-)" ->
        Wrap.a2 (-) (Try.float, Try.float) Encode.float (typeError "[number, number]")
          |> F2
          |> Ok

      "(*)" ->
        Wrap.a2 (*) (Try.float, Try.float) Encode.float (typeError "[number, number]")
          |> F2
          |> Ok

      "(/)" ->
        Wrap.a2 (/) (Try.float, Try.float) Encode.float (typeError "[number, number]")
          |> F2
          |> Ok

      "(//)" ->
        Wrap.a2 (//) (Try.int, Try.int) Encode.int (typeError "[integer, integer]")
          |> F2
          |> Ok

      "(^)" ->
        Wrap.a2 (^) (Try.float, Try.float) Encode.float (typeError "[number, number]")
          |> F2
          |> Ok

      "round" ->
        Wrap.a1 Basics.round Try.float Encode.int (typeError "[number]")
          |> F1
          |> Ok

      "floor" ->
        Wrap.a1 Basics.floor Try.float Encode.int (typeError "[number]")
          |> F1
          |> Ok

      "ceiling" ->
        Wrap.a1 Basics.ceiling Try.float Encode.int (typeError "[number]")
          |> F1
          |> Ok

      "truncate" ->
        Wrap.a1 Basics.truncate Try.float Encode.int (typeError "[number]")
          |> F1
          |> Ok

      "(==)" ->
        Err noCompareError

      _ ->
        Err notFoundError


dict : String -> Result String Function
dict fName =
  let
    notFoundError =
      "Function `"
      ++ fName
      ++ "` was not found in the `Dict` core library."

    typeError typeList =
      "Type error in arguments to `"
      ++ fName
      ++ "`: expected "
      ++ typeList
      ++ "."

  in
    case fName of
      "union" ->
        Wrap.a2 (Dict.union) (Try.dict, Try.dict) (Dict.toList >> Encode.object) (typeError "[object, object]")
          |> F2
          |> Ok

      "intersect" ->
        Wrap.a2 (Dict.intersect) (Try.dict, Try.dict) (Dict.toList >> Encode.object) (typeError "[object, object]")
          |> F2
          |> Ok

      "diff" ->
        Wrap.a2 (Dict.diff) (Try.dict, Try.dict) (Dict.toList >> Encode.object) (typeError "[object, object]")
          |> F2
          |> Ok

      _ ->
        Err notFoundError
