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
  = NotFound
  | Stuck SyntaxType 
{-
-}
smartNext : SyntaxTree -> Int -> Int
smartNext tree id =
  let 
    nextIterator i tree = 
      ((i, tree.name, Maybe.map (\((i, t, m), msg) -> (i, t)) (head tree.terms)), 
        if i == id then
          Err (Stuck (getType tree))
        else 
          findNext tree.terms
      )
    findNext indecies = 
      case indecies of
        (_, msg) :: (((i', t', m), _) as b) :: rest -> 
          case msg of
            Err (Stuck t) -> 
              if fst t == t' then 
                 Ok i'
              else
                case m of 
                  Just (i'', t'') -> 
                    if fst t == t'' then
                      Ok i''
                    else
                      Err (Stuck t)
                  Nothing ->
                    Ok i'
            Err NotFound -> findNext (b :: rest)
            Ok idx -> Ok idx
        [ (_, msg) ] -> msg
        [] -> Err NotFound 
    -- defaultVal i tree = 
    --   ( (i, tree.name)
    --   , Err NotFound
    --   , Maybe.map 
    --       (\(SubTree stree) -> (i - tree.size + stree.size, getType stree)) 
    --       (head (tree.terms))
    --   )
  in
     collect nextIterator tree 
        |> snd >> Result.withDefault id
    -- collectPath id (\tree -> Nothing) nextIterator tree |> withDefault id   

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
    search id prevIterator tree |> withDefault id   
