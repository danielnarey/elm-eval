module Eval.Call exposing
  ( Call
  , parse
  , fromLib
  )


-- Project
import Eval.Function exposing (Function(..))
import Eval.Resolve as Resolve
import Eval.Try as Try
import Eval.Try.List as TryList

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


{-| Given an interface to some Elm library, try to execute a `Call` and return
the result, providing an error message if the function is not found or the
arguments do not match.
-}
fromLib : (String -> Result String Function) -> Call -> Result String Value
fromLib lib call =
  case (call.f |> lib) of
    Ok f ->
      case f of
        F0 f0 ->
          call.args
            |> TryList.empty
            |> Result.fromMaybe (
              "The `"
              ++ call.f
              ++ "` function expects no arguments, but it got "
              ++ ( call.args |> List.length |> String.fromInt )
              ++ " instead."
            )
            |> Result.andThen f0

        F1 f1 ->
          call.args
            |> TryList.singleton
            |> Result.fromMaybe (
              "The `"
              ++ call.f
              ++ "` function expects 1 argument, but it got "
              ++ ( call.args |> List.length |> String.fromInt )
              ++ " instead."
            )
            |> Result.andThen f1

        F2 f2 ->
          call.args
            |> TryList.tuple2
            |> Result.fromMaybe (
              "The `"
              ++ call.f
              ++ "` function expects 2 arguments, but it got "
              ++ ( call.args |> List.length |> String.fromInt )
              ++ " instead."
            )
            |> Result.andThen f2

        F3 f3 ->
          call.args
            |> TryList.tuple3
            |> Result.fromMaybe (
              "The `"
              ++ call.f
              ++ "` function expects 3 arguments, but it got "
              ++ ( call.args |> List.length |> String.fromInt )
              ++ " instead."
            )
            |> Result.andThen f3

    Err e ->
      Err e
