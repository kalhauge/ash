module Ash.Command exposing (..)

import List exposing (map, any, head, member)
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

{-
Updates the subtree identified by the index. Returns an new index representing
the new subtree.
-}
update : (SyntaxTree -> SyntaxTree) -> Int -> SyntaxTree -> (Int, SyntaxTree)
update fn id =
  let 
    toTree {kind, terms} = syntax kind (List.map snd terms)
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


delete : Focus -> SyntaxTree -> (Int, SyntaxTree)
delete id tree =
  let
    deleteIterator i tree =
      if i == id then
         Nothing
      else 
        case allOf (tree.terms) of
          Just terms -> 
            Just <| 
              ( Maybe.withDefault 0 ( List.maximum (List.map fst terms))
              , setTerms (List.map snd terms) tree
              )
          Nothing -> 
            case onlyOne tree.terms of
              Just (i', tree') -> Just (i - tree.size + tree'.size, tree') 
              Nothing -> Nothing
  in collectPath id (\i tree -> Just (0, tree)) deleteIterator tree
      |> Maybe.withDefault (id, tree)


trim : Grammar -> SyntaxTree -> SyntaxTree
trim grammar =
  let
    trimmer i tree =
      Ash.Grammar.get tree.kind grammar
        |> flip Maybe.andThen (\alt -> 
            if List.length alt == 1 then 
              head (getTerms tree)
            else 
              Nothing
          )
        |> Maybe.withDefault tree 
  in 
    collectS trimmer

