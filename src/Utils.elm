module Utils where

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

