module Eval.Core exposing
  ( lib )


-- Project
import Eval.Core.Array
import Eval.Core.Basics
import Eval.Core.Bitwise
import Eval.Core.Char
import Eval.Core.Dict
import Eval.Core.List
import Eval.Core.Set
import Eval.Core.String
import Eval.Core.Tuple
import Eval.Function exposing (Function(..))


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
      case parts of
        [] ->
          ("Basics", "")

        first :: [] ->
          ("Basics", first)

        first :: rest ->
          case first of
            "always" ->
              ("Basics", expression)

            _ ->
              (first, rest |> String.join ".")

  in
    case moduleName of
      "Array" ->
        Eval.Core.Array.lib fName

      "Basics" ->
        Eval.Core.Basics.lib fName

      "Bitwise" ->
        Eval.Core.Bitwise.lib fName

      "Char" ->
        Eval.Core.Char.lib fName

      "Debug" ->
        Err "The Debug module is not available through this interface."

      "Dict" ->
        Eval.Core.Dict.lib fName

      "List" ->
        Eval.Core.List.lib fName

      "Maybe" ->
        Err "The Maybe module is not available through this interface."

      "Platform" ->
        Err "The Platform module is not available through this interface."

      "Platform.Cmd" ->
        Err "The Platform.Cmd module is not available through this interface."

      "Platform.Sub" ->
        Err "The Platform.Sub module is not available through this interface."

      "Process" ->
        Err "The Process module is not available through this interface."

      "Result" ->
        Err "The Result module is not available through this interface."

      "Set" ->
        Eval.Core.Set.lib fName

      "String" ->
        Eval.Core.String.lib fName

      "Task" ->
        Err "The Task module is not available through this interface."

      "Tuple" ->
        Eval.Core.Tuple.lib fName

      _ ->
        Err (
          "A module named `"
          ++ moduleName
          ++ "` was not found in Elm's core libraries. "
          ++ "Note that Elm module names are always capitalized and that "
          ++ "module and function names should be separated by a single `.` "
          ++ "(example: `String.length`)."
        )
