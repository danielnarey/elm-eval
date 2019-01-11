module Eval.Core.Basics exposing
  ( lib )


-- Project
import Eval.Core.Error as Error
import Eval.Core.List
import Eval.Encode as Encode
import Eval.Function exposing (Function(..))
import Eval.Try as Try
import Eval.Wrap as Wrap

-- Core
import Json.Encode exposing (Value)


lib : String -> Result String Function
lib fName =
  case fName of
    "(+)" ->
      Wrap.a2 (+) (Try.float, Try.float) Encode.float (Error.expected fName "[number, number]")
        |> F2
        |> Ok

    "(-)" ->
      Wrap.a2 (-) (Try.float, Try.float) Encode.float (Error.expected fName "[number, number]")
        |> F2
        |> Ok

    "(*)" ->
      Wrap.a2 (*) (Try.float, Try.float) Encode.float (Error.expected fName "[number, number]")
        |> F2
        |> Ok

    "(/)" ->
      Wrap.a2 (/) (Try.float, Try.float) Encode.float (Error.expected fName "[number, number]")
        |> F2
        |> Ok

    "(//)" ->
      Wrap.a2 (//) (Try.int, Try.int) Encode.int (Error.expected fName "[integer, integer]")
        |> F2
        |> Ok

    "(^)" ->
      Wrap.a2 (^) (Try.float, Try.float) Encode.float (Error.expected fName "[number, number]")
        |> F2
        |> Ok

    "round" ->
      Wrap.a1 Basics.round Try.float Encode.int (Error.expected fName "[number]")
        |> F1
        |> Ok

    "floor" ->
      Wrap.a1 Basics.floor Try.float Encode.int (Error.expected fName "[number]")
        |> F1
        |> Ok

    "ceiling" ->
      Wrap.a1 Basics.ceiling Try.float Encode.int (Error.expected fName "[number]")
        |> F1
        |> Ok

    "truncate" ->
      Wrap.a1 Basics.truncate Try.float Encode.int (Error.expected fName "[number]")
        |> F1
        |> Ok

    "(==)" ->
      Err (Error.noCompare fName)

    "(/=)" ->
      Err (Error.noCompare fName)

    "(<)" ->
      Err (Error.noCompare fName)

    "(>)" ->
      Err (Error.noCompare fName)

    "(<=)" ->
      Err (Error.noCompare fName)

    "(>=)" ->
      Err (Error.noCompare fName)

    "max" ->
      Err (Error.noCompare fName)

    "min" ->
      Err (Error.noCompare fName)

    "compare" ->
      Err (Error.noCompare fName)

    "not" ->
      Wrap.a1 Basics.not Try.bool Encode.bool (Error.expected fName "[boolean]")
        |> F1
        |> Ok

    "(&&)" ->
      Wrap.a2 (&&) (Try.bool, Try.bool) Encode.bool (Error.expected fName "[boolean, boolean]")
        |> F2
        |> Ok

    "(||)" ->
      Wrap.a2 (||) (Try.bool, Try.bool) Encode.bool (Error.expected fName "[boolean, boolean]")
        |> F2
        |> Ok

    "xor" ->
      Wrap.a2 Basics.xor (Try.bool, Try.bool) Encode.bool (Error.expected fName "[boolean, boolean]")
        |> F2
        |> Ok

    "(++)" ->
      Err (
        "The `(++)` function can't be called throught this interface because "
        ++ "Elm doesn't support type inference on JavaScript values passed in "
        ++ "through ports. Use `String.append` or `List.append` instead."
      )

    "modBy" ->
      Wrap.a2 Basics.modBy (Try.int, Try.int) Encode.int (Error.expected fName "[integer, integer]")
        |> F2
        |> Ok

    "remainderBy" ->
      Wrap.a2 Basics.remainderBy (Try.int, Try.int) Encode.int (Error.expected fName "[integer, integer]")
        |> F2
        |> Ok

    "negate" ->
      Wrap.a1 Basics.negate Try.float Encode.float (Error.expected fName "[number]")
        |> F1
        |> Ok

    "abs" ->
      Wrap.a1 Basics.abs Try.float Encode.float (Error.expected fName "[number]")
        |> F1
        |> Ok

    "clamp" ->
      Wrap.a3 Basics.clamp (Try.float, Try.float, Try.float) Encode.float (Error.expected fName "[number, number, number]")
        |> F3
        |> Ok

    "sqrt" ->
      Wrap.a1 Basics.sqrt Try.float Encode.float (Error.expected fName "[number]")
        |> F1
        |> Ok

    "logBase" ->
      Wrap.a2 Basics.logBase (Try.float, Try.float) Encode.float (Error.expected fName "[number, number]")
        |> F2
        |> Ok

    "e" ->
      Wrap.a0 (\() -> Basics.e) Encode.float
        |> F0
        |> Ok

    "degrees" ->
      Wrap.a1 Basics.degrees Try.float Encode.float (Error.expected fName "[number]")
        |> F1
        |> Ok

    "radians" ->
      Wrap.a1 Basics.radians Try.float Encode.float (Error.expected fName "[number]")
        |> F1
        |> Ok

    "turns" ->
      Wrap.a1 Basics.turns Try.float Encode.float (Error.expected fName "[number]")
        |> F1
        |> Ok

    "pi" ->
      Wrap.a0 (\() -> Basics.pi) Encode.float
        |> F0
        |> Ok

    "cos" ->
      Wrap.a1 Basics.cos Try.float Encode.float (Error.expected fName "[number]")
        |> F1
        |> Ok

    "sin" ->
      Wrap.a1 Basics.sin Try.float Encode.float (Error.expected fName "[number]")
        |> F1
        |> Ok

    "tan" ->
      Wrap.a1 Basics.tan Try.float Encode.float (Error.expected fName "[number]")
        |> F1
        |> Ok

    "acos" ->
      Wrap.a1 Basics.acos Try.float Encode.float (Error.expected fName "[number]")
        |> F1
        |> Ok

    "asin" ->
      Wrap.a1 Basics.asin Try.float Encode.float (Error.expected fName "[number]")
        |> F1
        |> Ok

    "atan" ->
      Wrap.a1 Basics.atan Try.float Encode.float (Error.expected fName "[number]")
        |> F1
        |> Ok

    "atan2" ->
      Wrap.a2 Basics.atan2 (Try.float, Try.float) Encode.float (Error.expected fName "[number]")
        |> F2
        |> Ok

    "toPolar" ->
      Wrap.a2 (\a b -> Basics.toPolar (a,b)) (Try.float, Try.float) (Encode.tuple2 (Encode.float, Encode.float)) (Error.expected fName "[number, number]")
        |> F2
        |> Ok

    "fromPolar" ->
      Wrap.a2 (\a b -> Basics.fromPolar (a,b)) (Try.float, Try.float) (Encode.tuple2 (Encode.float, Encode.float)) (Error.expected fName "[number, number]")
        |> F2
        |> Ok

    "isNaN" ->
      Wrap.a1 Basics.isNaN Try.float Encode.bool (Error.expected fName "[number]")
        |> F1
        |> Ok

    "isInfinite" ->
      Wrap.a1 Basics.isInfinite Try.float Encode.bool (Error.expected fName "[number]")
        |> F1
        |> Ok

    "identity" ->
      (\a -> Ok a)
        |> F1
        |> Ok

    "always" ->
      (\(a,_) -> Ok a)
        |> F2
        |> Ok

    "always.string" ->
      Wrap.a2 Basics.always (Try.string, Try.string) Encode.string (Error.expected fName "[string]")
        |> F2
        |> Ok

    "always.char" ->
      Wrap.a2 Basics.always (Try.char, Try.char) Encode.char (Error.expected fName "[string-1]")
        |> F2
        |> Ok

    "always.int" ->
      Wrap.a2 Basics.always (Try.int, Try.int) Encode.int (Error.expected fName "[integer]")
        |> F2
        |> Ok

    "always.float" ->
      Wrap.a2 Basics.always (Try.float, Try.float) Encode.float (Error.expected fName "[number]")
        |> F2
        |> Ok

    "always.list" ->
      Wrap.a2 Basics.always (Try.list, Try.list) Encode.list (Error.expected fName "[array]")
        |> F2
        |> Ok

    "always.array" ->
      Wrap.a2 Basics.always (Try.array, Try.array) Encode.array (Error.expected fName "[array]")
        |> F2
        |> Ok

    "always.dict" ->
      Wrap.a2 Basics.always (Try.dict, Try.dict) Encode.dict (Error.expected fName "[object]")
        |> F2
        |> Ok

    "(<|)" ->
      Err (Error.noFunction fName)

    "(|>)" ->
      Err (Error.noFunction fName)

    "(<<)" ->
      Err (Error.noFunction fName)

    "(>>)" ->
      Err (Error.noFunction fName)

    "(::)" ->
      Eval.Core.List.lib fName

    _ ->
      Err (
        Error.notFound "Basics" fName
        ++ " If you are trying to access a function in another core library, "
        ++ "the module name must be given first (example: `String.length`)."
      )
