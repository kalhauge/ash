module Utils exposing (..)

import Array exposing (Array)

prepend : a -> List a -> List a
prepend = (::)


tryPrepend : a -> Maybe (List a) -> Maybe (List a)
tryPrepend a maybe = 
  Maybe.map ((::) a) maybe


tryUpdate : (a -> Maybe a) -> List a ->  Maybe (List a)
tryUpdate f al = 
  let al' = List.map f al
  in case Maybe.oneOf al' of 
    Just _ -> Just <| List.map2 Maybe.withDefault al al'
    Nothing -> Nothing

allOf : List (Maybe a) -> Maybe (List a)
allOf maybes = 
  case maybes of
    [] ->
      Just []

    Just a :: rest -> 
      Maybe.map (\rest -> a :: rest) <| allOf rest 

    Nothing :: rest -> 
      Nothing

compress : List (Maybe a) -> List a
compress maybes = 
  case maybes of
    [] -> 
      []
    Just a :: rest -> 
      a :: compress rest
    Nothing :: rest -> 
      compress rest

maybeToList : Maybe a -> List a
maybeToList maybe = 
  Maybe.withDefault [] (Maybe.map (\a -> [a]) maybe) 

{- 
Lazy and short-ciruit oneOf with map.
-}
oneOfMap : (a -> Maybe b) -> List a -> Maybe b
oneOfMap fn list =
  case list of
    [] -> Nothing
    item :: rest -> 
      case fn item of
        Nothing -> oneOfMap fn rest
        a -> a
{- 
Lazy and short-ciruiting oneOf with fold.
-}
oneOfScan : (a -> b -> Result b c) -> b -> List a -> Maybe c
oneOfScan fn val list =
  case list of
    [] -> Nothing
    item :: rest ->
      case fn item val of
        Err val' -> oneOfScan fn val' rest
        Ok item -> Just item


onlyOne : List (Maybe a) -> Maybe a
onlyOne list = 
  case list of
    [] -> 
      Nothing

    Just a :: rest ->
      case Maybe.oneOf rest of 
        Just _ -> Nothing
        Nothing -> Just a

    Nothing :: rest ->
      onlyOne rest


last : List a -> Maybe a
last list = 
  case list of 
    [] -> Nothing
    [a] -> Just a
    a :: rest -> last rest

enumerate : List a -> List (Int, a) 
enumerate = List.indexedMap (,) 

(&>) = Maybe.andThen

arrayUpdate : Int -> (a -> a) -> Array a -> Maybe (Array a)
arrayUpdate i f array =
  Array.get i array &> 
    \x -> Just (Array.set i (f x) array)


