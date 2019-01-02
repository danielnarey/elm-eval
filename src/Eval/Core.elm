module Eval.Core exposing
  ( lib )


-- Project
import Eval.Function exposing (Function(..))
import Eval.Try as Try
import Eval.Wrap as Wrap
import Eval.Core.Basics
import Eval.Core.List
import Eval.Core.Array
import Eval.Core.Dict

-- Core
import Array
import Set
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

  in
    case moduleName of
      "Basics" ->
        Eval.Core.Basics.lib fName

      "List" ->
        Eval.Core.List.lib fName

      "Array" ->
        Eval.Core.Array.lib fName

      "Dict" ->
        Eval.Core.Dict.lib fName

      _ ->
        Err (
          "A module named `"
          ++ moduleName
          ++ "` was not found in Elm's core libraries. "
          ++ "Note that Elm module names are always capitalized and that "
          ++ "module and function names should be separated by a single `.` "
          ++ "(example: `String.length`)."
        )
