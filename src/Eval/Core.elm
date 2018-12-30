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
function that takes one or more `Json` values as arguments and returns a `Json`
value
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

    noFunctionError =
      "Functional operators like `(|>)` or `(>>)` can't be called throught "
      ++ "interface because only primitive types, arrays, and objects are "
      ++ "allowed as arguments. As an alternative, `elm-eval` function calls "
      ++ "can be chained on the JavaScript side using promises with `.then` or "
      ++ "`await`."

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

      "(/=)" ->
        Err noCompareError

      "(<)" ->
        Err noCompareError

      "(>)" ->
        Err noCompareError

      "(<=)" ->
        Err noCompareError

      "(>=)" ->
        Err noCompareError

      "max" ->
        Err noCompareError

      "min" ->
        Err noCompareError

      "compare" ->
        Err noCompareError

      "not" ->
        Wrap.a1 Basics.not Try.bool Encode.bool (typeError "[boolean]")
          |> F1
          |> Ok

      "(&&)" ->
        Wrap.a2 (&&) (Try.bool, Try.bool) Encode.bool (typeError "[boolean, boolean]")
          |> F2
          |> Ok

      "(||)" ->
        Wrap.a2 (||) (Try.bool, Try.bool) Encode.bool (typeError "[boolean, boolean]")
          |> F2
          |> Ok

      "xor" ->
        Wrap.a2 Basics.xor (Try.bool, Try.bool) Encode.bool (typeError "[boolean, boolean]")
          |> F2
          |> Ok

      "(++)" ->
        Err (
          "The `(++)` function can't be called throught this interface because "
          ++ "Elm doesn't support type inference from JavaScript values. "
          ++ "Use `String.append` or `List.append` instead."
        )

      "modBy" ->
        Wrap.a2 Basics.modBy (Try.int, Try.int) Encode.int (typeError "[integer, integer]")
          |> F2
          |> Ok

      "remainderBy" ->
        Wrap.a2 Basics.remainderBy (Try.int, Try.int) Encode.int (typeError "[integer, integer]")
          |> F2
          |> Ok

      "negate" ->
        Wrap.a1 Basics.negate Try.float Encode.float (typeError "[number]")
          |> F1
          |> Ok

      "abs" ->
        Wrap.a1 Basics.abs Try.float Encode.float (typeError "[number]")
          |> F1
          |> Ok

      "clamp" ->
        Wrap.a3 Basics.clamp (Try.float, Try.float, Try.float) Encode.float (typeError "[number, number, number]")
          |> F3
          |> Ok

      "sqrt" ->
        Wrap.a1 Basics.sqrt Try.float Encode.float (typeError "[number]")
          |> F1
          |> Ok

      "logBase" ->
        Wrap.a2 Basics.logBase (Try.float, Try.float) Encode.float (typeError "[number, number]")
          |> F2
          |> Ok

      "e" ->
        Wrap.a0 (\() -> Basics.e) Encode.float
          |> F0
          |> Ok

      "degrees" ->
        Wrap.a2 Basics.degrees (Try.float, Try.float) Encode.float (typeError "[number, number]")
          |> F2
          |> Ok

      "radians" ->
        Wrap.a2 Basics.radians (Try.float, Try.float) Encode.float (typeError "[number, number]")
          |> F2
          |> Ok

      "turns" ->
        Wrap.a2 Basics.turns (Try.float, Try.float) Encode.float (typeError "[number, number]")
          |> F2
          |> Ok

      "pi" ->
        Wrap.a0 (\() -> Basics.pi) Encode.float
          |> F0
          |> Ok

      "cos" ->
        Wrap.a1 Basics.cos Try.float Encode.float (typeError "[number]")
          |> F1
          |> Ok

      "sin" ->
        Wrap.a1 Basics.sin Try.float Encode.float (typeError "[number]")
          |> F1
          |> Ok

      "tan" ->
        Wrap.a1 Basics.tan Try.float Encode.float (typeError "[number]")
          |> F1
          |> Ok

      "acos" ->
        Wrap.a1 Basics.acos Try.float Encode.float (typeError "[number]")
          |> F1
          |> Ok

      "asin" ->
        Wrap.a1 Basics.asin Try.float Encode.float (typeError "[number]")
          |> F1
          |> Ok

      "atan" ->
        Wrap.a1 Basics.atan Try.float Encode.float (typeError "[number]")
          |> F1
          |> Ok

      "atan2" ->
        Wrap.a2 Basics.atan2 (Try.float, Try.float) Encode.float (typeError "[number]")
          |> F2
          |> Ok

      "toPolar" ->
        Wrap.a2 (\a b -> Basics.toPolar (a,b)) (Try.float, Try.float) (\(a,b) -> Encode.list Encode.float [a,b]) (typeError "[number, number]")
          |> F2
          |> Ok

      "fromPolar" ->
        Wrap.a2 (\a b -> Basics.fromPolar (a,b)) (Try.float, Try.float) (\(a,b) -> Encode.list Encode.float [a,b]) (typeError "[number, number]")
          |> F2
          |> Ok

      "isNaN" ->
        Wrap.a1 Basics.isNaN Try.float Encode.bool (typeError "[number]")
          |> F1
          |> Ok

      "isInfinite" ->
        Wrap.a1 Basics.isInfinite Try.float Encode.bool (typeError "[number]")
          |> F1
          |> Ok

      "identity" ->
        (\a -> Ok a)
          |> F1
          |> Ok

      "always" ->
        (\a _ -> Ok a)
          |> F2
          |> Ok

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
