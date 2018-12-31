module Eval.Try exposing
  ( field
  , string
  , int
  , float
  , bool
  , list
  , listString
  , listInt
  , listFloat
  , listBool
  , listList
  , listDict
  , dict
  , empty
  , singleton
  , tuple2
  , tuple3
  )


import Dict exposing (Dict)
import Json.Decode as Decode exposing (Value)


field : String -> Value -> Maybe Value
field key =
  Decode.decodeValue (Decode.field key Decode.value)
    >> Result.toMaybe


string : Value -> Maybe String
string =
  Decode.decodeValue Decode.string
    >> Result.toMaybe


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

list : Value -> Maybe (List Value)
list =
  Decode.decodeValue (Decode.list Decode.value)
    >> Result.toMaybe


listString : Value -> Maybe (List String)
listString =
  Decode.decodeValue (Decode.list Decode.string)
    >> Result.toMaybe


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


listDict : Value -> Maybe (List (Dict String Value))
listDict =
  Decode.decodeValue (Decode.list (Decode.dict Decode.value))
    >> Result.toMaybe


dict : Value -> Maybe (Dict String Value)
dict =
  Decode.decodeValue (Decode.dict Decode.value)
    >> Result.toMaybe


empty : List Value -> Maybe ()
empty ls =
  case ls of
    [] ->
      Just ()
    _ ->
      Nothing


singleton : List Value -> Maybe Value
singleton ls =
  case ls of
    first :: [] ->
      Just first
    _ ->
      Nothing


tuple2 : List Value -> Maybe (Value, Value)
tuple2 ls =
  case (ls, ls |> List.drop 1) of
    (first :: rest, second :: []) ->
      Just (first, second)
    (_, _) ->
      Nothing


tuple3 : List Value -> Maybe (Value, Value, Value)
tuple3 ls =
  case (ls, ls |> List.drop 1, ls |> List.drop 2) of
    (first :: slice1, second :: slice2, third :: []) ->
      Just (first, second, third)
    (_, _, _) ->
      Nothing
