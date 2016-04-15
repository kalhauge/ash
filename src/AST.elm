module AST where

import List exposing (sum, map2)

import Grammar exposing (..)
import Utils exposing (..)


type alias SNode a
  = { name : ClauseId
    , alt : AlternativeId 
    , terms : List a
    , size : Int
    }

-- Recusive type to please the TypeChecker 
type SubTree = SubTree (SNode SubTree)

type alias SyntaxTree = SNode SubTree

unfix : SubTree -> SyntaxTree 
unfix (SubTree a) = a

map : (a -> b) -> SNode a -> SNode b
map fn a = 
  { a | terms = List.map fn a.terms }


getTerms : SyntaxTree -> List SyntaxTree 
getTerms {terms} = 
  List.map unfix terms


mapS : (List SyntaxTree -> List b) -> SyntaxTree -> SNode b
mapS fn a =
  { a | terms = fn (getTerms a) }


setTerms : List SyntaxTree -> SNode a -> SyntaxTree
setTerms terms inner =
  syntax (inner.name) (inner.alt) terms


getType : SNode a -> (ClauseId, AlternativeId) 
getType {name, alt} =
  (name, alt)


syntax : 
  ClauseId 
  -> AlternativeId
  -> List SyntaxTree 
  -> SyntaxTree
syntax name alt terms =
  { name = name
  , alt = alt
  , terms = List.map SubTree terms
  , size = sum (List.map .size terms) + 1
  }

-- Special functions from here

subIndecies : Int -> SyntaxTree -> List Int
subIndecies id tree =
  indecies (id - tree.size) (getTerms tree)

subWithIndecies : Int -> SyntaxTree -> List (Int, SyntaxTree)
subWithIndecies id tree =
  map2 (,) (subIndecies id tree) (getTerms tree)
  
indecies : Int -> List SyntaxTree -> List Int
indecies i list = 
  case list of
    [] -> 
      []
    term :: rest -> 
      let index = i + term.size
      in index :: indecies index rest


type alias TreeIterator a = Int -> SyntaxTree -> Maybe a

{- 
Will iterate through the tree in index order and return the
first positive result.
-}
first : TreeIterator a -> SyntaxTree -> Maybe a
first itr =
  let 
    step term i =
      case iterate i term of
        Nothing -> Err <| i + term.size
        Just a -> Ok a
    iterate i tree = 
      case oneOfScan step i (getTerms tree) of
        Nothing -> 
          itr (i + tree.size) tree 
        a -> a
  in 
    iterate 0 


{- 
Will iterate through the tree directly towards the target
and return the first positive result.
-}
search : Int -> TreeIterator a -> SyntaxTree -> Maybe a
search id itr =
  let 
    step term i =
      if i < id && id <= i + term.size then
        case iterate i term of
          Nothing -> Err <| i + term.size
          Just a -> Ok a
      else 
        Err (i + term.size)
    iterate i tree = 
      case oneOfScan step i (getTerms tree) of
        Nothing -> 
          itr (i + tree.size) tree 
        a -> a
  in 
    iterate 0 


type alias TreeCollector a = Int -> SNode a -> a 

{-
Collect runs through the syntax tree and collects the results in a
SNode.
-}
collect : TreeCollector a -> SyntaxTree -> a
collect clt =
  let 
    step i terms =
      case terms of 
        [] -> []
        term :: rest ->
          iterate i term :: step (term.size + i) rest 
    iterate i tree = 
      clt (i + tree.size) (mapS (step i) tree)
  in 
    iterate 0 

collectS : (Int -> SyntaxTree -> SyntaxTree) -> SyntaxTree -> SyntaxTree
collectS clt tree = 
  unfix <| collect (\i t -> SubTree <| clt i t) tree


{-
CollectPath only touches the nodes that are direct parrents
to the indcies given. Because of this we need a way to map a 
SyntaxTree to the collectValue.
-}
collectPath : Int -> (Int -> SyntaxTree -> a) -> TreeCollector a -> SyntaxTree -> a
collectPath id default clt =
  let 
    step i terms =
      case terms of 
        [] -> []
        term :: rest ->
          if i < id && id <= i + term.size then
            iterate i term :: step (term.size + i) rest 
          else 
            default (i + term.size) term :: step (term.size + i) rest
    iterate i tree = 
      clt (i + tree.size) (mapS (step i) tree)
  in 
    iterate 0 


-- Should be put some where else.

match : 
  Alternative
  -> List a 
  -> (String -> a)
  -> Maybe (List a)
match template terms f =
  case (template, terms) of
    (Ref name :: template', term:: terms') ->
        match template' terms' f
         |> tryPrepend term
    
    (Lex str :: template', _) ->
        match template' terms f
         |> tryPrepend (f str) 
    
    ([], []) ->
      Just []
    
    _ ->
      Nothing

translate : 
  Grammar 
  -> SNode a 
  -> (String -> a)
  -> Maybe (List a)
translate grammar {name, alt, terms} f =
  Grammar.get name alt grammar 
    `Maybe.andThen` (\alt -> match alt terms f)

