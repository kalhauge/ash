module Command where

import List exposing (map, any, head, member)
import Maybe exposing (withDefault)

import AST exposing (..)
import Utils exposing (..)
import Grammar exposing (..)

{-
Return the subtree, represneted by the index
-}
get : Int -> SyntaxTree -> Maybe SyntaxTree
get id =
  let get' i tree = 
    if i == id then 
      Just tree 
    else 
      Nothing
  in search id get' 

{-
Updates the subtree identified by the index. Returns an new index representing
the new subtree.
-}
update : Int -> (SyntaxTree -> SyntaxTree) -> SyntaxTree -> (Int, SyntaxTree)
update id fn =
  let 
    toTree = AST.map (snd >> SubTree) 
    update' i node =
      if id == i then
        let rval = fn (toTree node)
        in (i - node.size + rval.size, rval)
      else 
        let idx = Maybe.withDefault 0 (
            List.maximum (List.map fst node.terms)
          )
        in (idx, toTree node)
  in collectPath id (\i tree -> (0, tree)) update'


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
     search id childIterator tree |> withDefault id 

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
    search id parrentIterator tree |> withDefault id   
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
    search id nextIterator tree |> withDefault id   

type MoveErr
  = GiveUp
  | Stuck
  | ChildStuck

type SNInfo = SNInfo Int SyntaxType (Maybe Int)
{-
-}
smartNext : SyntaxTree -> Int -> Int
smartNext tree id =
  let
    helper ((SNInfo i t _), msg) = i
    nextIterator i tree = 
      ( SNInfo i (getType tree) (Maybe.map helper (head tree.terms))
      , if i == id then
          Err Stuck
        else 
          findNext (getType tree) tree.terms
      )
    findNext pt indecies = 
      case indecies of
        (SNInfo i t m, msg) :: ((SNInfo i' t' m', _) as b) :: rest -> 
          case msg of
            Err Stuck -> 
              -- Does the next field have a child:
              case m' of 
                Just i'' -> 
                  -- If the next has the same type as the parrent take the
                  -- child if possible (RR) 
                  if pt == t' then
                    Ok i''
                  else  
                    Ok i' 
                Nothing -> Ok i'

            Err ChildStuck  -> 
              -- In this case is the first term's child stuck
              if t == pt then
                Ok i'
              else 
                Err GiveUp

            Err GiveUp -> 
              findNext pt (b :: rest)

            Ok idx -> 
              Ok idx
        
        [ (_, msg) ] -> 
          case msg of
            Err Stuck -> Err ChildStuck
            a -> a
        
        [] -> 
          Err GiveUp 
  in
     collect nextIterator tree 
        |> snd >> Result.withDefault id

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
    search id prevIterator tree |> withDefault id   

smartPrev : SyntaxTree -> Int -> Int
smartPrev tree id =
  let
    helper ((SNInfo i t _), msg) = i
    prevIterator i tree = 
      ( SNInfo i (getType tree) (Maybe.map helper (last tree.terms))
      , if i == id then
          Err Stuck
        else 
          findPrev (getType tree) tree.terms
      )
    findPrev pt indecies = 
      case indecies of
        (SNInfo i t m, msg) :: ((SNInfo i' t' m', msg') as b) :: rest -> 
          case msg' of
            Err Stuck -> 
              -- Does the next field have a child:
              case m of 
                Just i'' -> 
                  -- If the next has the same type as the parrent take the
                  -- child if possible (LR) 
                  if pt == t then
                    Ok i''
                  else  
                    Ok i 
                Nothing -> Ok i

            Err ChildStuck  -> 
              -- In this case is the first term's child stuck
              if t' == pt then
                Ok i
              else 
                Err GiveUp

            Err GiveUp -> 
              case msg of
                Ok idx -> Ok idx
                Err Stuck -> Err ChildStuck
                _ -> findPrev pt (b :: rest)

            Ok idx -> 
              Ok idx
        
        [ (_, msg) ] -> 
          case msg of
            Err Stuck -> Err ChildStuck
            a -> a
        
        [] -> 
          Err GiveUp 
  in
     collect prevIterator tree 
        |> snd >> Result.withDefault id

-- smartPrev : SyntaxTree -> Int -> Int
-- smartPrev tree id =
--   let 
--     prevIterator i tree = 
--       let subs = (subWithIndecies i tree)
--       in case prev' subs of
--         Just (idx, sub) -> 
--           if getType sub == getType tree then
--              last (subIndecies idx sub)
--           else Just idx
--         Nothing -> Nothing
--     prev' indecies = 
--       case indecies of
--         a :: b :: rest -> 
--           if id == fst b then Just a
--           else prev' rest
--         _ -> Nothing
--   in
--     search id prevIterator tree |> withDefault id   
