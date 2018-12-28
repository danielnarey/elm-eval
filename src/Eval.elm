module Eval exposing
  ( Call
  , parse
  , call
  , callFromLib
  )


{-| Evaluate an Elm function call, given a record with fields for the function
name (as a string) and a list of arguments (encoded as `Json` values)

@docs Call
@docs parse
@docs call
@docs callFromLib
-}


-- Project
import Eval.Core as Core
import Eval.Function exposing (Function(..))
import Eval.Resolve as Resolve
import Eval.Try as Try
-- Core
import Json.Encode exposing (Value)


{-| Represents an Elm function call
-}
type alias Call =
  { f : String
  , args : List Value
  }


{-| Parse a `Json` object as a `Call`, resolving errors in the `f` field to an
empty string and resoving errors in the `args` field to an empty list
-}
parse : Value -> Call
parse object =
  { f = object |> Try.field "f" |> Resolve.string
  , args = object |> Try.field "args" |> Resolve.list
  }


{-| Try to execute a `Call` and return the result, providing an error message
if the function is not found or the arguments do not match. This is a shortcut
to execute calls to functions in Elm's core libraries.
-}
call : Call -> Result String Value
call =
  callFromLib Core.lib


{-| This generic version of `call` is included to allow the `elm-eval` package
to be extended by modules that provide access to other libraries of Elm
functions. See the `Eval.Core` module for implementation details.
-}
callFromLib : (String -> Result String Function) -> Call -> Result String Value
callFromLib lib op =
  case (op.f |> lib) of
    Ok f ->
      case f of
        F0 f0 ->
          op.args
            |> Try.empty
            |> Result.fromMaybe (
              "The `"
              ++ op.f
              ++ "` function expects no arguments, but it got "
              ++ ( op.args |> List.length |> String.fromInt )
              ++ "instead."
            )
            |> Result.andThen f0

        F1 f1 ->
          op.args
            |> Try.singleton
            |> Result.fromMaybe (
              "The `"
              ++ op.f
              ++ "` function expects 1 argument, but it got "
              ++ ( op.args |> List.length |> String.fromInt )
              ++ "instead."
            )
            |> Result.andThen f1

        F2 f2 ->
          op.args
            |> Try.tuple2
            |> Result.fromMaybe (
              "The `"
              ++ op.f
              ++ "` function expects 2 arguments, but it got "
              ++ ( op.args |> List.length |> String.fromInt )
              ++ "instead."
            )
            |> Result.andThen f2

        F3 f3 ->
          op.args
            |> Try.tuple3
            |> Result.fromMaybe (
              "The `"
              ++ op.f
              ++ "` function expects 3 arguments, but it got "
              ++ ( op.args |> List.length |> String.fromInt )
              ++ "instead."
            )
            |> Result.andThen f3

    Err e ->
      Err e
