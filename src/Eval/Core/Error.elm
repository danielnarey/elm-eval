module Eval.Core.Error exposing
  ( notFound
  , noCompare
  , noFunction
  , expected
  )


notFound moduleName fName =
  "A function named `"
  ++ fName
  ++ "` was not found in the `"
  ++ moduleName
  ++ "` core library."

noCompare fName =
  "Comparison functions like `"
  ++ fName
  ++ "` can't be called through this interface because Elm doesn't support "
  ++ "type inference on JavaScript values passed in through ports."

noFunction fName =
  "Functions like `"
  ++ fName
  ++ "` that take other functions as arguments can't be called through this "
  ++ "interface because Elm only allows primitive types, arrays, and objects "
  ++ "to be passed in through ports. As an alternative, you could chain "
  ++ "`elm-eval` function calls on the JavaScript side using promise chains "
  ++ "or aync/await syntax."

expected fName typeList =
  "Type error in arguments to `"
  ++ fName
  ++ "`: expected "
  ++ typeList
  ++ "."
