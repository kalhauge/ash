module Ash.Parser exposing (parse, suggest)

import List 
import Array
import String
import Lazy.List as Lizt exposing (LazyList)

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
parse grammar entry str = 
  suggest grammar entry str 
  |> Lizt.head
  |> Debug.log "parse"


suggest : Grammar -> ClauseId -> String -> LazyList SyntaxTree
suggest grammar entry str = 
  suggestRule grammar entry str 
  |> Lizt.map fst
  |> Debug.log ("suggest " ++ entry)

type alias Suggest a = LazyList (a, String)

map : (a -> b) -> Suggest a -> Suggest b
map f =
  Lizt.map (\(a, str) -> (f a, str))

andThen : Suggest a -> (a -> String -> Suggest b) -> Suggest b
andThen sug f =
  Lizt.flatMap (\(a, str) -> f a str) sug

batch : List (Suggest a) -> Suggest a
batch list = 
  Lizt.flatten (Lizt.fromList list)

only : a -> String -> Suggest a
only a str =
  Lizt.singleton (a, str)

empty : Suggest a 
empty = Lizt.empty

{-

-}
suggestRule : Grammar -> ClauseId -> String -> Suggest SyntaxTree
suggestRule g clause str = 
  if String.isEmpty str then
    empty
  else
    case Grammar.getRule clause g of
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
    leftTree = batch <| List.map (suggestKind g str) real
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

suggestKind : 
  Grammar 
  -> String
  -> (SyntaxKind, List Term) 
  -> Suggest SyntaxTree
suggestKind g str (kind, terms) = 
  map (SyntaxTree.syntax kind) 
    (suggestTerms' (Grammar.isLexical (fst kind)) g str terms) 

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
suggestTerms' isLexical g str terms =
  let 
    helper term sugest = sugest `andThen` \trees str -> 
        case term of
          Lex lex -> 
            if String.startsWith lex str then
              only trees (String.dropLeft (String.length lex) str)
            else if String.startsWith str lex then
              only trees ""
            else
              empty
          Ref clause -> 
            if String.isEmpty str then 
              if isLexical then
                empty
              else
                only (SyntaxTree.empty :: trees) str
            else
              map (\t -> t :: trees) (suggestRule g clause str)
  in
    List.foldl helper (only [] <| if isLexical then str else String.trimLeft str) terms
    |> map (List.reverse)


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
