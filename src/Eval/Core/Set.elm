module Eval.Core.Set exposing
  ( lib )


-- Project
import Eval.Core.Error as Error
import Eval.Core.List
import Eval.Encode as Encode
import Eval.Function exposing (Function(..))
import Eval.Try as Try
import Eval.Wrap as Wrap

-- Core
import Set
import Json.Encode exposing (Value)


lib : String -> Result String Function
lib fName =
  case fName of
    "empty" ->
      Wrap.a0 (\() -> Set.empty) (Set.toList >> Encode.list)
        |> F0
        |> Ok

    "singleton" ->
      Err (Error.noCompare fName)

    "singleton.string" ->
      Wrap.a1 Set.singleton Try.string Encode.setString (Error.expected fName "[string]")
        |> F1
        |> Ok

    "singleton.char" ->
      Wrap.a1 Set.singleton Try.char Encode.setChar (Error.expected fName "[string]")
        |> F1
        |> Ok

    "singleton.int" ->
      Wrap.a1 Set.singleton Try.int Encode.setInt (Error.expected fName "[integer]")
        |> F1
        |> Ok

    "singleton.float" ->
      Wrap.a1 Set.singleton Try.float Encode.setFloat (Error.expected fName "[number]")
        |> F1
        |> Ok

    "insert" ->
      Err (Error.noCompare fName)

    "insert.string" ->
      Wrap.a2 Set.insert (Try.string, Try.setString) Encode.setString (Error.expected fName "[string, array(string)]")
        |> F2
        |> Ok

    "insert.char" ->
      Wrap.a2 Set.insert (Try.char, Try.setChar) Encode.setChar (Error.expected fName "[string-1, array(string-1)]")
        |> F2
        |> Ok

    "insert.int" ->
      Wrap.a2 Set.insert (Try.int, Try.setInt) Encode.setInt (Error.expected fName "[integer, array(integer)]")
        |> F2
        |> Ok

    "insert.float" ->
      Wrap.a2 Set.insert (Try.float, Try.setFloat) Encode.setFloat (Error.expected fName "[float, array(number)]")
        |> F2
        |> Ok

    "remove" ->
      Err (Error.noCompare fName)

    "remove.string" ->
      Wrap.a2 Set.remove (Try.string, Try.setString) Encode.setString (Error.expected fName "[string, array(string)]")
        |> F2
        |> Ok

    "remove.char" ->
      Wrap.a2 Set.remove (Try.char, Try.setChar) Encode.setChar (Error.expected fName "[string-1, array(string-1)]")
        |> F2
        |> Ok

    "remove.int" ->
      Wrap.a2 Set.remove (Try.int, Try.setInt) Encode.setInt (Error.expected fName "[integer, array(integer)]")
        |> F2
        |> Ok

    "remove.float" ->
      Wrap.a2 Set.remove (Try.float, Try.setFloat) Encode.setFloat (Error.expected fName "[float, array(number)]")
        |> F2
        |> Ok

    "isEmpty" ->
      Eval.Core.List.lib fName

    "member" ->
      Err (Error.noCompare fName)

    "member.string" ->
      Wrap.a2 Set.member (Try.string, Try.setString) Encode.bool (Error.expected fName "[string, array(string)]")
        |> F2
        |> Ok

    "member.char" ->
      Wrap.a2 Set.member (Try.char, Try.setChar) Encode.bool (Error.expected fName "[string-1, array(string-1)]")
        |> F2
        |> Ok

    "member.int" ->
      Wrap.a2 Set.member (Try.int, Try.setInt) Encode.bool (Error.expected fName "[integer, array(integer)]")
        |> F2
        |> Ok

    "member.float" ->
      Wrap.a2 Set.member (Try.float, Try.setFloat) Encode.bool (Error.expected fName "[float, array(number)]")
        |> F2
        |> Ok

    "size" ->
      Err (Error.noCompare fName)

    "size.string" ->
      Wrap.a1 Set.size Try.setString Encode.int (Error.expected fName "array(string)]")
        |> F1
        |> Ok

    "size.char" ->
      Wrap.a1 Set.size Try.setChar Encode.int (Error.expected fName "[array(string-1)]")
        |> F1
        |> Ok

    "size.int" ->
      Wrap.a1 Set.size Try.setInt Encode.int (Error.expected fName "[array(integer)]")
        |> F1
        |> Ok

    "size.float" ->
      Wrap.a1 Set.size Try.setFloat Encode.int (Error.expected fName "[array(number)]")
        |> F1
        |> Ok

    "union" ->
      Err (Error.noCompare fName)

    "union.string" ->
      Wrap.a2 Set.union (Try.setString, Try.setString) Encode.setString (Error.expected fName "[array(string), array(string)]")
        |> F2
        |> Ok

    "union.char" ->
      Wrap.a2 Set.union (Try.setChar, Try.setChar) Encode.setChar (Error.expected fName "[array(string-1), array(string-1)]")
        |> F2
        |> Ok

    "union.int" ->
      Wrap.a2 Set.union (Try.setInt, Try.setInt) Encode.setInt (Error.expected fName "[array(integer), array(integer)]")
        |> F2
        |> Ok

    "union.float" ->
      Wrap.a2 Set.union (Try.setFloat, Try.setFloat) Encode.setFloat (Error.expected fName "[array(number), array(number)]")
        |> F2
        |> Ok

    "intersect" ->
      Err (Error.noCompare fName)

    "intersect.string" ->
      Wrap.a2 Set.intersect (Try.setString, Try.setString) Encode.setString (Error.expected fName "[array(string), array(string)]")
        |> F2
        |> Ok

    "intersect.char" ->
      Wrap.a2 Set.intersect (Try.setChar, Try.setChar) Encode.setChar (Error.expected fName "[array(string-1), array(string-1)]")
        |> F2
        |> Ok

    "intersect.int" ->
      Wrap.a2 Set.intersect (Try.setInt, Try.setInt) Encode.setInt (Error.expected fName "[array(integer), array(integer)]")
        |> F2
        |> Ok

    "intersect.float" ->
      Wrap.a2 Set.intersect (Try.setFloat, Try.setFloat) Encode.setFloat (Error.expected fName "[array(number), array(number)]")
        |> F2
        |> Ok

    "diff" ->
      Err (Error.noCompare fName)

    "diff.string" ->
      Wrap.a2 Set.diff (Try.setString, Try.setString) Encode.setString (Error.expected fName "[array(string), array(string)]")
        |> F2
        |> Ok

    "diff.char" ->
      Wrap.a2 Set.diff (Try.setChar, Try.setChar) Encode.setChar (Error.expected fName "[array(string-1), array(string-1)]")
        |> F2
        |> Ok

    "diff.int" ->
      Wrap.a2 Set.diff (Try.setInt, Try.setInt) Encode.setInt (Error.expected fName "[array(integer), array(integer)]")
        |> F2
        |> Ok

    "diff.float" ->
      Wrap.a2 Set.diff (Try.setFloat, Try.setFloat) Encode.setFloat (Error.expected fName "[array(number), array(number)]")
        |> F2
        |> Ok

    "toList" ->
      Err (Error.noCompare fName)

    "toList.string" ->
      lib "fromList.string"

    "toList.char" ->
      lib "fromList.char"

    "toList.int" ->
      lib "fromList.int"

    "toList.float" ->
      lib "fromList.float"

    "fromList" ->
      Err (Error.noCompare fName)

    "fromList.string" ->
      Wrap.a1 identity Try.setString Encode.setString (Error.expected fName "[array(string)]")
        |> F1
        |> Ok

    "fromList.char" ->
      Wrap.a1 identity Try.setChar Encode.setChar (Error.expected fName "[array(string-1)]")
        |> F1
        |> Ok

    "fromList.int" ->
      Wrap.a1 identity Try.setInt Encode.setInt (Error.expected fName "[array(integer)]")
        |> F1
        |> Ok

    "fromList.float" ->
      Wrap.a1 identity Try.setFloat Encode.setFloat (Error.expected fName "[array(number)]")
        |> F1
        |> Ok

    "map" ->
      Err (Error.noFunction fName)

    "foldl" ->
      Err (Error.noFunction fName)

    "foldr" ->
      Err (Error.noFunction fName)

    "filter" ->
      Err (Error.noFunction fName)

    "partition" ->
      Err (Error.noFunction fName)

    _ ->
      Err (Error.notFound "Set" fName)
