module Ash.Parser exposing (parse, suggest)

import List 
import Array
import String

import Utils exposing (..)

import Ash.Grammar as Grammar exposing 
  ( Grammar
  , ClauseId
  , SyntaxKind
  , Term(..)
  , getRule
  )
import Ash.SyntaxTree as SyntaxTree exposing (syntax, SyntaxTree)

parse : Grammar -> ClauseId -> String -> Maybe SyntaxTree
parse grammar entry str= 
  suggest grammar entry str 
  |> List.head


suggest : Grammar -> ClauseId -> String -> List SyntaxTree
suggest grammar entry str = 
  suggestRule grammar entry str 
  |> List.map fst
  |> Debug.log ("suggest " ++ entry)

type alias Suggest a = List (a, String)

map : (a -> b) -> Suggest a -> Suggest b
map f =
  List.map (\(a, str) -> (f a, str))

andThen : Suggest a -> (a -> String -> Suggest b) -> Suggest b
andThen sug f =
  List.concatMap (\(a, str) -> f a str) sug

batch : List (Suggest a) -> Suggest a
batch list = 
  List.concat list

only : a -> String -> Suggest a
only a str =
  [(a, str)]

empty : Suggest a 
empty = []

{-

-}
suggestRule : Grammar -> ClauseId -> String -> Suggest SyntaxTree
suggestRule g clause str = 
  Debug.log ("rule! " ++ clause)  <| case Grammar.getRule clause g of
    Just alts ->
      Array.toIndexedList alts
      |> suggestAlts g clause str
    Nothing -> empty 
    
{- 
Given a list
-}
suggestAlts : 
  Grammar 
  -> ClauseId 
  -> String 
  -> List (Int, List Term) 
  -> Suggest SyntaxTree
suggestAlts g clause str alts = 
  let 
    (recursive, real) = devideAlts clause alts 
    leftTree : Suggest SyntaxTree
    leftTree = batch <| Debug.log "leftTree" <| List.map (suggestKind g str) real
  in 
    batch 
      [ leftTree
      , leftTree `andThen` \tree str' ->
        recursive
        |> List.map (\(kind, terms) -> 
            map 
              (SyntaxTree.syntax kind << prepend tree) 
              (suggestTerms' (not <| Grammar.isLexical clause) g str' terms) 
          )
        |> batch
      ]
    |> Debug.log ("batch " ++ clause)

suggestKind : 
  Grammar 
  -> String
  -> (SyntaxKind, List Term) 
  -> Suggest SyntaxTree
suggestKind g str (kind, terms) = 
  map (SyntaxTree.syntax kind) 
    (suggestTerms' (not <| Grammar.isLexical (fst kind)) g str terms) 

{- 
Suggest takes a grammar, a list of tokens to parse and somthing to parse,
and then returns a list of posible syntax trees with holes in them, and envtuel 
unparsed string.

Suggest only works as syntaxtical rules. This means that it assumes that any space
can be between the terms.
-} 
suggestTerms : Grammar -> String -> List Term -> Suggest (List SyntaxTree)
suggestTerms = 
  suggestTerms' True

suggestLexicalTerms : Grammar -> String -> List Term -> Suggest (List SyntaxTree)
suggestLexicalTerms = 
  suggestTerms' False

suggestTerms' : Bool -> Grammar -> String -> List Term -> Suggest (List SyntaxTree)
suggestTerms' b g str terms =
  let str = if b then String.trimLeft str else str in
  Debug.log ("suggest " ++ str) <| case Debug.log "terms" terms of 
    Lex lex :: rest ->
      if String.startsWith lex str then
        suggestTerms' b g (String.dropLeft (String.length lex) str) rest 
      else if String.startsWith str lex then
        only 
          ((List.map (always SyntaxTree.empty) <| Grammar.clauseIds rest))
          ""
      else
        empty
    Ref rule :: rest -> 
      suggestRule g rule str `andThen` \tree str' -> 
      map (prepend tree) (suggestTerms' b g str rest)
    [] -> 
      only [] str


devideAlts : 
  ClauseId 
  -> List (Int, List Term) 
  -> (List (SyntaxKind, List Term), List (SyntaxKind, List Term))
devideAlts clause = 
  let 
    devide (i, alt) = 
      let kind = (clause, i) in
      case alt of
        Ref clause' :: rest -> 
          if clause == clause' then
            Err (kind, rest)
            -- left recursive!!
          else
            Ok (kind, alt)
        _ -> Ok (kind, alt)
  in 
    List.map devide 
    >> Utils.partitionResults
