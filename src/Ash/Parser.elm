module Ash.Parser exposing (parse, suggest)

import List 
import Array
import String

import Lazy.List as Lizt exposing (LazyList, (+++), (:::))
import Lazy exposing (Lazy)

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

suggest : Grammar -> ClauseId -> String -> LazyList SyntaxTree
suggest grammar entry str = 
  suggestRule grammar entry str 
  |> Lizt.keepIf (snd >> Maybe.map (String.isEmpty) >> Maybe.withDefault True)
  |> Lizt.map fst

type alias Suggest a = LazyList (a, Maybe String)

map : (a -> b) -> Suggest a -> Suggest b
map f =
  Lizt.map (\(a, str) -> (f a, str))

andThen : Suggest a -> (a -> Maybe String -> Suggest b) -> Suggest b
andThen sug f =
  Lizt.flatMap (\(a, str) -> f a str) sug

batch : LazyList (Suggest a) -> Suggest a
batch = 
  Lizt.flatten 

forever : (a -> Maybe String -> Suggest a) -> Suggest a -> Suggest a
forever fn base = Lazy.lazy <| \() -> 
  case Lazy.force base of 
    Lizt.Nil -> Lizt.Nil 
    Lizt.Cons a rest -> 
      Lazy.force <| base +++ forever fn (base `andThen` fn)

only : a -> Maybe String -> Suggest a
only a str =
  Lizt.singleton (a, str)

empty : Suggest a 
empty = Lizt.empty

{-

-}
suggestRule : Grammar -> ClauseId -> String -> Suggest SyntaxTree
suggestRule g clause str = 
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
    leftTree = batch <| Lizt.map (suggestKind g str) real
  in 
    flip forever leftTree <| \tree msg ->
      case msg of 
        Just str' ->
          recursive
          |> Lizt.map (\(kind, terms) -> 
              map 
                (SyntaxTree.syntax kind << prepend tree) 
                (suggestTerms' (Grammar.isLexical clause) g str' terms) 
            )
          |> batch
        Nothing ->
          empty

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
  suggestTerms' False

suggestLexicalTerms : Grammar -> String -> List Term -> Suggest (List SyntaxTree)
suggestLexicalTerms = 
  suggestTerms' True

suggestTerms' : Bool -> Grammar -> String -> List Term -> Suggest (List SyntaxTree)
suggestTerms' isLexical g str terms =
  let 
    helper term suggest = suggest `andThen` \trees msg -> 
      case term of
        Lex lex -> 
          case msg of 
            Just str -> 
              if String.startsWith lex str then
                only trees <| Maybe.map (String.dropLeft (String.length lex)) msg
              else if String.startsWith str lex then
                only trees Nothing
              else
                empty
            Nothing -> 
              only trees Nothing
        Ref clause -> 
          case msg of
            Just str -> 
              if String.isEmpty str then 
                if isLexical then
                  empty
                else
                  only (SyntaxTree.empty :: trees) msg
              else
                map (\t -> t :: trees) (suggestRule g clause str)
            Nothing -> 
              only (SyntaxTree.empty :: trees) msg
  in
    List.foldl helper 
      (only [] <| Just <| if isLexical then str else String.trimLeft str) terms
    |> map (List.reverse )


devideAlts : 
  ClauseId 
  -> List (Int, List Term) 
  -> (LazyList (SyntaxKind, List Term), LazyList (SyntaxKind, List Term))
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
    >> \(xs, ys) -> (Lizt.fromList xs, Lizt.fromList ys)

