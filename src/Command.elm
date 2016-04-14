module Command where

import List exposing (map, any, head, member)
import Maybe exposing (withDefault)

import AST exposing (..)
import Utils exposing (..)


{-
FocusIn deepens the  
-}
child : SyntaxTree -> Int -> Int
child tree id =
  let childIterator i tree = 
    if i == id then
      head (subIndecies i tree) 
    else
      Nothing
  in 
     first childIterator tree |> withDefault id 

{-
Focus out takes and identifier and returns
the idetifier for the tree just above it.
-}
parrent : SyntaxTree -> Int -> Int
parrent tree id =
  let parrentIterator i tree = 
    if member id (subIndecies i tree) then
      Just i
    else 
      Nothing
  in
    first parrentIterator tree |> withDefault id   
    -- If we can't find a parrent, chill

{-
-}
next : SyntaxTree -> Int -> Int
next tree id =
  let 
    nextIterator i tree = next' (subIndecies i tree)
    next' indecies = 
      case indecies of
        a :: b :: rest -> 
          if id == a then Just b
          else next' rest
        _ -> Nothing
  in
    first nextIterator tree |> withDefault id   
{-
-}
smartNext : SyntaxTree -> Int -> Int
smartNext tree id =
  let 
    nextIterator i tree = 
      let subs = (subWithIndecies i tree)
      in 
        case next' subs of
        Just (idx, sub) -> 
          if getType sub == getType tree then
             head (subIndecies idx sub)
          else Just idx
        Nothing -> Nothing
    next' indecies = 
      case indecies of
        a :: b :: rest -> 
          if id == fst a then Just b
          else next' rest
        _ -> Nothing
  in
    first nextIterator tree |> withDefault id   

prev : SyntaxTree -> Int -> Int
prev tree id =
  let 
    prevIterator i tree = prev' (subIndecies i tree)
    prev' indecies = 
      case indecies of
        a :: b :: rest -> 
          if id == b then Just a
          else prev' rest
        _ -> Nothing
  in
    first prevIterator tree |> withDefault id   

smartPrev : SyntaxTree -> Int -> Int
smartPrev tree id =
  let 
    prevIterator i tree = 
      let subs = (subWithIndecies i tree)
      in case prev' subs of
        Just (idx, sub) -> 
          if getType sub == getType tree then
             last (subIndecies idx sub)
          else Just idx
        Nothing -> Nothing
    prev' indecies = 
      case indecies of
        a :: b :: rest -> 
          if id == fst b then Just a
          else prev' rest
        _ -> Nothing
  in
    first prevIterator tree |> withDefault id   
