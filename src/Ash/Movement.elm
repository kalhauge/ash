module Ash.Movement exposing 
  ( Direction(..)
  , moveSmartFocus
  , moveFocus
  )

import Ash.SyntaxTree as SyntaxTree exposing ( SyntaxTree, Focus )
import Ash.Grammar as Grammar exposing ( SyntaxKind )

import List exposing (head)

import Utils

type Direction 
  = Child
  | Parrent
  | Next
  | Prev

moveSmartFocus : Direction -> Focus -> SyntaxTree -> Focus 
moveSmartFocus dir focus st =
  let mover = 
    case dir of
      Child -> 
        smartChild 
      Parrent -> 
        smartParrent
      Next -> 
        smartNext
      Prev -> 
        smartPrev
  in mover st focus
 
moveFocus : Direction -> Focus -> SyntaxTree -> Focus
moveFocus dir focus st =
  let mover = 
    case dir of
      Child -> 
        child 
      Parrent -> 
        parrent
      Next -> 
        next
      Prev -> 
        prev
  in mover st focus 

{- child deepens the  -}
child : SyntaxTree -> Int -> Int
child tree id =
  let childIterator i tree = 
    if i == id then
      head (SyntaxTree.subIndecies i tree) 
    else
      Nothing
  in 
     SyntaxTree.search id childIterator tree 
     |> Maybe.withDefault id 

{-
Focus out takes and identifier and returns
the idetifier for the tree just above it.
-}
parrent : SyntaxTree -> Int -> Int
parrent tree id =
  let parrentIterator i tree = 
    if List.member id (SyntaxTree.subIndecies i tree) then
      Just i
    else 
      Nothing
  in
    SyntaxTree.search id parrentIterator tree 
    |> Maybe.withDefault id   

{-
-}
next : SyntaxTree -> Int -> Int
next tree id =
  let 
    nextIterator i tree = next' (SyntaxTree.subIndecies i tree)
    next' indecies = 
      case indecies of
        a :: b :: rest -> 
          if id == a then Just b
          else next' (b :: rest)
        _ -> Nothing
  in
    SyntaxTree.search id nextIterator tree 
    |> Maybe.withDefault id   


prev : SyntaxTree -> Int -> Int
prev tree id =
  let 
    prevIterator i tree = 
      prev' (SyntaxTree.subIndecies i tree)
    prev' indecies = 
      case indecies of
        a :: b :: rest -> 
          if id == b then Just a
          else prev' (b :: rest)
        _ -> Nothing
  in
    SyntaxTree.search id prevIterator tree 
    |> Maybe.withDefault id   

{- Smart movement ahead Be aware here be dragons -} 

type MoveErr
  = GiveUp
  | Stuck
  | ChildStuck

type SNInfo = SNInfo Int SyntaxKind (Maybe Int)

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
      ( SNInfo i (tree.kind) (head (SyntaxTree.subIndecies i tree))
      , Err GiveUp 
      )
  in
     SyntaxTree.collectPath id defaultVal nextIterator tree 
    |> snd >> Result.withDefault id

smartPrev : SyntaxTree -> Int -> Int
smartPrev tree id =
  let
    helper ((SNInfo i t _), msg) = i
    prevIterator i tree = 
      ( SNInfo i (tree.kind) (Maybe.map helper (Utils.last tree.terms))
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
      ( SNInfo i (tree.kind) (Utils.last (SyntaxTree.subIndecies i tree))
      , Err GiveUp 
      )

  in
     SyntaxTree.collectPath id defaultVal prevIterator tree 
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
    SyntaxTree.collectPath id (\i tree -> Nothing) upIterator tree 
    |> Maybe.map fst >> Maybe.withDefault id

smartChild : SyntaxTree -> Int -> Int
smartChild tree id = 
  let
    downIterator i tree =
      if i == id then
        goInWithType (tree.kind) (id - tree.size) tree
      else
        Nothing
    goInWithType t i tree 
      =
      case head (SyntaxTree.getTerms tree) of
        Just tree' -> 
          if tree'.kind == t then
             case goInWithType t i tree' of
               Nothing -> Just (i + tree'.size)
               a -> a
          else
            Just (i + tree'.size)
        Nothing -> Nothing
  in 
    SyntaxTree.search id downIterator tree 
    |> Maybe.withDefault id
