module Eval.Encode exposing
  ( value
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


import Array exposing (Array)
import Set exposing (Set)
import Dict exposing (Dict)
import Json.Encode as Encode exposing (Value)


value : Value -> Value
value v =
  v


string : String -> Value
string =
  Encode.string


char : Char -> Value
char =
  String.fromChar >> Encode.string


int : Int -> Value
int =
  Encode.int


float : Float -> Value
float =
  Encode.float


bool : Bool -> Value
bool =
  Encode.bool


tuple2 : (a -> Value, b -> Value) -> (a, b) -> Value
tuple2 (da, db) (a, b) =
  [da a, db b]
    |> Encode.list (\v -> v)


tuple3 : (a -> Value, b -> Value, c -> Value) -> (a, b, c) -> Value
tuple3 (da, db, dc) (a, b, c) =
  [da a, db b, dc c]
    |> Encode.list (\v -> v)


list : List Value -> Value
list =
  Encode.list (\v -> v)


listString : List String -> Value
listString =
  Encode.list Encode.string


listChar : List Char -> Value
listChar =
  Encode.list (String.fromChar >> Encode.string)


listInt : List Int -> Value
listInt =
  Encode.list Encode.int


listFloat : List Float -> Value
listFloat =
  Encode.list Encode.float


listBool : List Bool -> Value
listBool =
  Encode.list Encode.bool


listList : List (List Value) -> Value
listList =
  Encode.list (Encode.list (\v -> v))


listTuple2 : (a -> Value, b -> Value) -> List (a, b) -> Value
listTuple2 (da, db) =
  Encode.list (\(a,b) -> [da a, db b] |> Encode.list (\v -> v))


listTuple3 : (a -> Value, b -> Value, c -> Value) -> List (a, b, c) -> Value
listTuple3 (da, db, dc) =
  Encode.list (\(a,b,c) -> [da a, db b, dc c] |> Encode.list (\v -> v))


array : Array Value -> Value
array =
  Encode.array (\v -> v)


arrayString : Array String -> Value
arrayString =
  Encode.array Encode.string


arrayChar : Array Char -> Value
arrayChar =
  Encode.array (String.fromChar >> Encode.string)


arrayInt : Array Int -> Value
arrayInt =
  Encode.array Encode.int


arrayFloat : Array Float -> Value
arrayFloat =
  Encode.array Encode.float


setString : Set String -> Value
setString =
  Encode.set Encode.string


setChar : Set Char -> Value
setChar =
  Encode.set (String.fromChar >> Encode.string)


setInt : Set Int -> Value
setInt =
  Encode.set Encode.int


setFloat : Set Float -> Value
setFloat =
  Encode.set Encode.float

dict : Dict String Value -> Value
dict =
  Dict.toList >> Encode.object
