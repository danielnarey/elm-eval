module Eval exposing
  ( call
  , parse
  , coreLib
  )


{-| This package provides an infrastructure for parsing and evaluating Elm
function calls that have been passed in as data through ports.

@docs call
@docs parse
@docs coreLib
-}


-- Project
import Eval.Function exposing (Function(..))
import Eval.Call as Call exposing (Call)
import Eval.Core as Core

-- Core
import Json.Encode exposing (Value)


{-| Given an interface to some Elm library, try to execute a `Call` and return
the result, providing an error message if the function is not found or the
arguments do not match.

Example with a function from Elm's core libraries:

    { f = "(+)"
    , args =
      [ 1 |> Json.Encode.int
      , 2 |> Json.Encode.int
      ]
    }
      |> Eval.call Eval.coreLib

    --> Ok (Json.Encode.Value 3)


Example with a function from a user library called `Greeting` that has a
function named `hello`:

      { f = "hello"
      , args = [ "World" |> Json.Encode.string ]
      }
        |> Eval.call Greeting.lib

      --> Ok (Json.Encode.Value "Hello, World!")

-}
call : (String -> Result String Function) -> Call -> Result String Value
call =
  Call.fromLib


{-| Parse a `Json` object as a `Call`, resolving errors in the `f` field to an
empty string and resoving errors in the `args` field to an empty list

    jsObject
      |> Eval.parse
      |> Eval.call Eval.coreLib

-}
parse : Value -> Call
parse =
  Call.parse


{-| An interface to Elm's core libraries (can be passed as the first argument
to `call`)
-}
coreLib : String -> Result String Function
coreLib =
  Core.lib
