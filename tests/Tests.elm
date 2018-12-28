module Tests exposing (..)

-- Project
import Eval
-- Test
import Expect exposing (Expectation)
import Test exposing (..)
-- Core
import Json.Encode as Encode


-- HELPERS --



-- TESTS --


suite : Test
suite =
  [ basics
    |> describe "Call functions from `Basics` core library"

  ]
    |> describe "Testing `Eval.elm`"


basics : List Test
basics =
  [ ( \() ->
      { f = "(+)"
      , args =
        [ 1 |> Encode.int
        , 2 |> Encode.int
        ]
      }
        |> Eval.call
        |> Result.withDefault (Encode.null)
        |> Expect.equal (3 |> Encode.int)
    )
      |> test "(+) 1 2"

  , ( \() ->
      { f = "(*)"
      , args =
        [ 3.25 |> Encode.float
        , -1 |> Encode.int
        ]
      }
        |> Eval.call
        |> Result.withDefault (Encode.null)
        |> Expect.equal (-3.25 |> Encode.float)
    )
      |> test "(*) 3.25 -1"

  , ( \() ->
      { f = "(/)"
      , args =
        [ -1 |> Encode.int
        , 0 |> Encode.int
        ]
      }
        |> Eval.call
        |> Result.withDefault (Encode.null)
        |> Expect.equal ((-1/0) |> Encode.float)
    )
      |> test "(/) -1 0"

  ]
