module Eval.Wrap exposing
  ( a0, a1, a2, a3 )


import Json.Decode exposing (Value)


a0 : (() -> o) -> (o -> Value) -> () -> Result String Value
a0 f encoder null =
  f null
    |> encoder
    |> Ok


a1 : (a -> o) -> (Value -> Maybe a) -> (o -> Value) -> String -> Value -> Result String Value
a1 f da encoder errorMsg a =
  Maybe.map f (da a)
    |> Maybe.map encoder
    |> Result.fromMaybe errorMsg


a2 : (a -> b -> o) -> (Value -> Maybe a, Value -> Maybe b) -> (o -> Value) -> String -> (Value, Value) -> Result String Value
a2 f (da, db) encoder errorMsg (a, b)=
  Maybe.map2 f (da a) (db b)
    |> Maybe.map encoder
    |> Result.fromMaybe errorMsg


a3 : (a -> b -> c -> o) -> (Value -> Maybe a, Value -> Maybe b, Value -> Maybe c) -> (o -> Value) -> String -> (Value, Value, Value) -> Result String Value
a3 f (da, db, dc) encoder errorMsg (a, b, c)=
  Maybe.map3 f (da a) (db b) (dc c)
    |> Maybe.map encoder
    |> Result.fromMaybe errorMsg
