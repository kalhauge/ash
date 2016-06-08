module Ash.Command exposing (..)
import List exposing (map, any, head, member)
import Array exposing (Array)
import Maybe exposing (withDefault)

import Ash.SyntaxTree exposing (..)
import Ash.Grammar as Grammar exposing (..)

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


replaceL l i = 
  List.repeat (List.length l) i

updatetor : 
  TreeCollector (Maybe SubTree) 
  -> TreeCollector (Int -> (Maybe SubTree, List Int))
updatetor clt oldId tree itrId = 
  let 
    helper : 
      List (Int -> (Maybe SubTree, List Int))
      -> Int 
      -> (List (Maybe SubTree), List Int, Int)
    helper list id = 
      case list of 
        el :: rest -> 
          case el id of
            (Just (SubTree {size} as st), mapping) -> 
              let (list, mapping', nid) = helper rest (id + size) 
              in (Just st :: list, mapping ++ mapping', nid)
            (Nothing, mapping)  -> 
              let (list, mapping', nid) = helper rest id
              in (Nothing :: list, (replaceL mapping nid) ++ mapping', nid)
        [] -> ([], [], id)

    (terms, mapping', newId) = helper tree.terms itrId 
  in
     case clt oldId { tree | terms = terms } of
       Just (SubTree {size} as st)  -> 
         (Just st, mapping' ++ [ itrId + size ])
       Nothing -> 
         (Nothing,  mapping' ++ [ newId + 1] )


updatetor2 : 
  TreeCollector SubTree 
  -> TreeCollector (Int -> (SubTree, List Int))
updatetor2 clt oldId tree itrId = 
  let 
    helper : 
      List (Int -> (SubTree, List Int))
      -> Int 
      -> (List SyntaxTree, List Int, Int)
    helper list id = 
      case list of 
        e :: rest -> 
          case e id of 
            (SubTree {size} as st, mapping) -> 
              let (list, mapping', nid) = helper rest (id + size) 
              in (unfix st :: list, mapping ++ mapping', nid)
        [] -> ([], [], id)

    (terms, mapping', newId) = helper tree.terms itrId 
  in
    let st = unfix <| clt oldId (setTerms terms tree)
    in 
      if st.kind == tree.kind 
      then 
        -- If the tree is the same; repspect the mapping and the sizes.
        (SubTree st, mapping' ++ [ itrId + st.size ])  
      else 
        -- If the tree is not the same, all subtrees point to this 
        -- tree.
        (SubTree st, List.repeat tree.size (itrId + st.size))

         

update : TreeCollector (Maybe SubTree) -> SyntaxTree -> Maybe (SyntaxTree, Int -> Int)
update clt st =
  let 
    (result, map) = 
      collect (updatetor clt) st 0
    array = Array.fromList map
    mapping i = 
      Array.get (i - 1) array 
      |> Maybe.withDefault i
  in Maybe.map (\r -> (unfix r, mapping)) result

update2 : TreeCollector SubTree -> SyntaxTree -> Maybe (SyntaxTree, Int -> Int)
update2 clt st =
  let 
    (result, map) = 
      collect (updatetor2 clt) st 0
    array = Array.fromList map
    mapping i = 
      Array.get (i - 1) array 
      |> Maybe.withDefault i
  in Just (unfix result, mapping)

liftMaybe : TreeCollector SubTree -> TreeCollector (Maybe SubTree)
liftMaybe clt i tree= 
  Utils.allOf tree.terms 
  |> Maybe.map (List.map unfix >> flip setTerms tree >> clt i) 

replacer : Focus -> (SyntaxTree -> SyntaxTree) -> TreeCollector SubTree
replacer id fn i tree = 
  if i == id then
     SubTree (fn tree)  
  else 
     SubTree tree

replace : 
  Focus 
  -> (SyntaxTree -> SyntaxTree) 
  -> SyntaxTree 
  -> Maybe (SyntaxTree, Int -> Int)
replace id fn tree =
  update2 (replacer id fn) tree

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
  Grammar.get tree.kind grammar
    |> flip Maybe.andThen (\alt -> 
        if List.length alt == 1 then 
          head tree.terms 
        else 
          Nothing
      )
    |> Maybe.withDefault (SubTree tree)


trim : Grammar -> SyntaxTree -> SyntaxTree
trim grammar st =
  collectS (trimmer grammar) st

empties : Grammar -> ClauseId -> SyntaxTree -> List (Int, ClauseId)
empties grammar clauseid tree =
  let
    collector : TreeCollector (Result Int (List (Int, ClauseId)))
    collector id tree = 
      if isEmpty tree then
        Err id
      else
        Ok (
          List.concat
          <| Utils.joinResults
          <| List.map2 
             (\sc -> Result.formatError (\id -> [(id, sc)]))
             (Grammar.subClauses grammar tree.kind)
             (tree.terms)
        )
  in 
    case collect collector tree of
      Ok list -> list
      Err id -> [(id, clauseid)]
