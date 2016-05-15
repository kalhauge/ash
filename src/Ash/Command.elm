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


delete : Int -> SyntaxTree -> (Int, SyntaxTree)
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
          else next' (b :: rest)
        _ -> Nothing
  in
    search id nextIterator tree |> withDefault id   


prev : SyntaxTree -> Int -> Int
prev tree id =
  let 
    prevIterator i tree = 
      prev' (subIndecies i tree)
    prev' indecies = 
      case indecies of
        a :: b :: rest -> 
          if id == b then Just a
          else prev' (b :: rest)
        _ -> Nothing
  in
    search id prevIterator tree |> withDefault id   

{- Smart movement ahead 
  Be aware here be dragons
-} 

type MoveErr
  = GiveUp
  | Stuck
  | ChildStuck

type SNInfo = SNInfo Int SyntaxId (Maybe Int)

{-
-}
smartNext : SyntaxTree -> Int -> Int
smartNext tree id =
  let
    helper ((SNInfo i t _), msg) = i
    nextIterator i tree = 
      ( SNInfo i (tree.kind) (Maybe.map helper (head tree.terms))
      , if i == id then
          Err Stuck
        else 
          findNext (tree.kind) tree.terms
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
            Err ChildStuck -> Err GiveUp
            a -> a
        
        [] -> 
          Err GiveUp 
    defaultVal i tree = 
      ( SNInfo i (tree.kind) (head (subIndecies i tree))
      , Err GiveUp 
      )
  in
     collectPath id defaultVal nextIterator tree 
        |> snd >> Result.withDefault id
smartPrev : SyntaxTree -> Int -> Int
smartPrev tree id =
  let
    helper ((SNInfo i t _), msg) = i
    prevIterator i tree = 
      ( SNInfo i (tree.kind) (Maybe.map helper (last tree.terms))
      , if i == id then
          Err Stuck
        else 
          findPrev (tree.kind) tree.terms
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
            Err ChildStuck -> Err GiveUp
            a -> a
        
        [] -> 
          Err GiveUp 
    defaultVal i tree = 
      ( SNInfo i (tree.kind) (last (subIndecies i tree))
      , Err GiveUp 
      )

  in
     collectPath id defaultVal prevIterator tree 
        |> snd >> Result.withDefault id

smartParrent : SyntaxTree -> Int -> Int
smartParrent tree id = 
  let
    upIterator i tree =
      let t = tree.kind
      in if i == id then
        Just (i, Just t)
      else
        case Maybe.oneOf tree.terms of
          Just (i', Just t') -> 
            if t' == t || i' == id then
              Just (i, Just t)
            else 
              Just (i', Nothing)
          a -> a
  in 
    collectPath id (\i tree -> Nothing) upIterator tree 
      |> Maybe.map fst >> Maybe.withDefault id

smartChild : SyntaxTree -> Int -> Int
smartChild tree id = 
  let
    downIterator i tree =
      if i == id then
        goInWithType (tree.kind) (id - tree.size) tree
      else
        Nothing
    goInWithType t i tree =
      case head (getTerms tree) of
        Just tree' -> 
          if tree'.kind == t then
             case goInWithType t i tree' of
               Nothing -> Just (i + tree'.size)
               a -> a
          else
            Just (i + tree'.size)
        Nothing -> Nothing
  in 
    search id downIterator tree |> Maybe.withDefault id


