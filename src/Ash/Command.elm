module Ash.Command exposing (..)
import List exposing (map, any, head, member)
import Array exposing (Array)
import Maybe exposing (withDefault)

import Ash.SyntaxTree exposing (..)
import Ash.Grammar exposing (..)

import Utils exposing (..)

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

updatetor : 
  TreeCollector (Maybe SubTree) 
  -> TreeCollector (Array Int -> Int -> (Maybe SubTree, Array Int))
updatetor clt oldId tree array itrId = 
  let 
    helper : 
      List (Array Int -> Int -> (Maybe SubTree, Array Int))
      -> Array Int 
      -> Int 
      -> (List (Maybe SubTree), Array Int, Int)
    helper list array id = 
      case list of 
        el :: rest -> 
          case el array id of
            (Just (SubTree {size} as st), arr) -> 
              let (list, array', nid) = helper rest arr (id + size) 
              in (Just st :: list, array', nid)
            (Nothing, arr)  -> 
              let (list, array', nid) = helper rest arr id
              in (Nothing :: list, array', nid)
        [] -> ([], array, id)

    (terms, array', newId) = helper tree.terms array itrId 
  in
    Debug.log "update" <| 
    case clt oldId { tree | terms = terms } of
      Nothing -> -- Tree Deleted 
       (Nothing, array)
      Just st -> 
        (Just st, Array.set oldId (newId + 1) array')
  

update : TreeCollector (Maybe SubTree) -> SyntaxTree -> Maybe (SyntaxTree, Int -> Int)
update clt st =
  let 
    (result, array) = 
      collect (updatetor clt) st (Array.repeat st.size 0) 0
    mapping i = 
      Array.get i (Debug.log ("mapping " ++ toString i) array) 
      |> Maybe.withDefault i
  in Maybe.map (\r -> (unfix r, mapping)) result

deleteIterator : Focus -> TreeCollector (Maybe SubTree)
deleteIterator id i tree =
  if i == id then
     Nothing
  else 
    case allOf (tree.terms) of
      Just terms -> 
        Just <| setTermsS terms tree
      Nothing -> 
        onlyOne tree.terms

delete : Focus -> SyntaxTree -> Maybe (SyntaxTree, Int -> Int)
delete id tree =
  update (deleteIterator id) tree

trimmer : Grammar -> TreeCollector SubTree
trimmer grammar i tree =
  Ash.Grammar.get tree.kind grammar
    |> flip Maybe.andThen (\alt -> 
        if List.length alt == 1 then 
          head tree.terms 
        else 
          Nothing
      )
    |> Maybe.withDefault (SubTree tree)


trim : Grammar -> SyntaxTree -> SyntaxTree
trim grammar =
  collectS (trimmer grammar) 

