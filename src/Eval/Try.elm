module Eval.Try exposing
  ( field
  , string
  , char
  , int
  , float
  , bool
  , tuple2
  , tuple3
  , list
  , listString
  , listChar
  , listInt
  , listFloat
  , listBool
  , listList
  , listTuple2
  , listTuple3
  , listKeyValue
  , listDict
  , array
  , arrayString
  , arrayChar
  , arrayInt
  , arrayFloat
  , setString
  , setChar
  , setInt
  , setFloat
  , dict
  )


-- Project
import Eval.Try.List as TryList

-- Core
import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)


field : String -> Value -> Maybe Value
field key =
  Decode.decodeValue (Decode.field key Decode.value)
    >> Result.toMaybe


string : Value -> Maybe String
string =
  Decode.decodeValue Decode.string
    >> Result.toMaybe


char : Value -> Maybe Char
char =
  let
    singleChar s =
      case (s |> String.toList) of
        first :: [] ->
          Just first
        _ ->
          Nothing

  in
    string >> Maybe.andThen singleChar


int : Value -> Maybe Int
int =
  Decode.decodeValue Decode.int
    >> Result.toMaybe


float : Value -> Maybe Float
float =
  Decode.decodeValue Decode.float
    >> Result.toMaybe


bool : Value -> Maybe Bool
bool =
  Decode.decodeValue Decode.bool
    >> Result.toMaybe


tuple2 : Value -> Maybe (Value, Value)
tuple2 =
  list >> Maybe.andThen TryList.tuple2


tuple3 : Value -> Maybe (Value, Value, Value)
tuple3 =
  list >> Maybe.andThen TryList.tuple3


list : Value -> Maybe (List Value)
list =
  Decode.decodeValue (Decode.list Decode.value)
    >> Result.toMaybe


listString : Value -> Maybe (List String)
listString =
  Decode.decodeValue (Decode.list Decode.string)
    >> Result.toMaybe


listChar : Value -> Maybe (List Char)
listChar =
  let
    resolveMaybes ls =
      case (ls |> List.member Nothing) of
        True ->
          Nothing

        False ->
          ls
            |> List.map (Maybe.withDefault '!')
            |> Just

  in
    Decode.decodeValue (Decode.list (Decode.value |> Decode.map char))
      >> Result.toMaybe
      >> Maybe.andThen resolveMaybes


listInt : Value -> Maybe (List Int)
listInt =
  Decode.decodeValue (Decode.list Decode.int)
    >> Result.toMaybe


listFloat : Value -> Maybe (List Float)
listFloat =
  Decode.decodeValue (Decode.list Decode.float)
    >> Result.toMaybe


listBool : Value -> Maybe (List Bool)
listBool =
  Decode.decodeValue (Decode.list Decode.bool)
    >> Result.toMaybe


listList : Value -> Maybe (List (List Value))
listList =
  Decode.decodeValue (Decode.list (Decode.list Decode.value))
    >> Result.toMaybe


listTuple2 : Value -> Maybe (List (Value, Value))
listTuple2 =
  let
    resolveMaybes ls =
      case (ls |> List.member Nothing) of
        True ->
          Nothing

        False ->
          ls
            |> List.map (Maybe.withDefault (Encode.null, Encode.null))
            |> Just

  in
    listList
      >> Maybe.map (List.map TryList.tuple2)
      >> Maybe.andThen resolveMaybes


listTuple3 : Value -> Maybe (List (Value, Value, Value))
listTuple3 =
  let
    resolveMaybes ls =
      case (ls |> List.member Nothing) of
        True ->
          Nothing

        False ->
          ls
            |> List.map (Maybe.withDefault (Encode.null, Encode.null, Encode.null))
            |> Just

  in
    listList
      >> Maybe.map (List.map TryList.tuple3)
      >> Maybe.andThen resolveMaybes


listKeyValue : Value -> Maybe (List (String, Value))
listKeyValue =
  let
    resolveMaybes ls =
      case (ls |> List.member Nothing) of
        True ->
          Nothing

        False ->
          ls
            |> List.map (Maybe.withDefault ("", Encode.null))
            |> Just

  in
    listList
      >> Maybe.map (List.map TryList.keyValue)
      >> Maybe.andThen resolveMaybes


listDict : Value -> Maybe (List (Dict String Value))
listDict =
  Decode.decodeValue (Decode.list (Decode.dict Decode.value))
    >> Result.toMaybe


array : Value -> Maybe (Array Value)
array =
  Decode.decodeValue (Decode.array Decode.value)
    >> Result.toMaybe


arrayString : Value -> Maybe (Array String)
arrayString =
  Decode.decodeValue (Decode.array Decode.string)
    >> Result.toMaybe


arrayChar : Value -> Maybe (Array Char)
arrayChar =
  listChar
    >> Maybe.map Array.fromList


arrayInt : Value -> Maybe (Array Int)
arrayInt =
  Decode.decodeValue (Decode.array Decode.int)
    >> Result.toMaybe


arrayFloat : Value -> Maybe (Array Float)
arrayFloat =
  Decode.decodeValue (Decode.array Decode.float)
    >> Result.toMaybe


setString : Value -> Maybe (Set String)
setString =
  Decode.decodeValue (Decode.list Decode.string)
    >> Result.toMaybe
    >> Maybe.map Set.fromList


setChar : Value -> Maybe (Set Char)
setChar =
  listChar
    >> Maybe.map Set.fromList


setInt : Value -> Maybe (Set Int)
setInt =
  Decode.decodeValue (Decode.list Decode.int)
    >> Result.toMaybe
    >> Maybe.map Set.fromList


setFloat : Value -> Maybe (Set Float)
setFloat =
  Decode.decodeValue (Decode.list Decode.float)
    >> Result.toMaybe
    >> Maybe.map Set.fromList


dict : Value -> Maybe (Dict String Value)
dict =
  Decode.decodeValue (Decode.dict Decode.value)
    >> Result.toMaybe
