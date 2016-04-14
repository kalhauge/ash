module AST where

import List exposing (sum)

import Grammar exposing (..)
import Utils exposing (..)


type alias SNode a
  = { name : ClauseId
    , alt : AlternativeId 
    , terms : a
    , size : Int
    }

-- Recusive type to please the TypeChecker 
type SFix = SFix (List (SNode SFix))

unFix : SFix -> List (SNode SFix)
unFix (SFix a) = a

type alias SyntaxTree = SNode SFix

map : (a -> b) -> SNode a -> SNode b
map fn a = 
  { a | terms = fn a.terms }

mapS : (List SyntaxTree -> b) -> SyntaxTree -> SNode b
mapS fn a =
  map (unFix >> fn) a

getTerms : SyntaxTree -> List SyntaxTree 
getTerms {terms} = 
  unFix terms


setTerms : List SyntaxTree -> SyntaxTree -> SyntaxTree
setTerms terms inner =
  syntax (inner.name) (inner.alt) terms


updateTerms : 
  (List SyntaxTree -> List SyntaxTree) 
  -> SyntaxTree 
  -> SyntaxTree
updateTerms fn tree =
  getTerms tree
    |> fn
    |> flip setTerms tree


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
  , terms = SFix terms
  , size = sum (List.map .size terms) + 1
  }

-- Special functions from here

subIndecies : Int -> SyntaxTree -> List Int
subIndecies id tree =
  indecies (id - tree.size) (getTerms tree)

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
    step i terms =
      case terms of 
        [] -> 
          Nothing
        term :: rest ->
          case iterate i term of
            Nothing -> step (term.size + i) rest 
            a -> a
    iterate i tree = 
      case step i (getTerms tree) of
        Nothing -> itr (i + tree.size) tree
        a -> a
  in 
    iterate 0 

type alias TreeCollector a = Int -> SNode (List a) -> a 

collect : TreeCollector a -> SyntaxTree -> a
collect clt =
  let 
    step i terms =
      case terms of 
        [] ->
          []
        term :: rest ->
          iterate i term :: step (term.size + i) rest 
    iterate i tree = 
      clt (i + tree.size) (mapS (step i) tree)
  in 
    iterate 0 
  


{-
Will iterate directly towards a index, and will return the first
positive result.
-}
-- directTo : Int -> TreeIterator a -> SyntaxTree -> Maybe a 
-- directTo target fn id tree =
--   case compare id target of
--     LT -> getTerms id  
--     EQ -> fn id tree
--     GT -> Nothing


get : Int -> SyntaxTree -> Maybe SyntaxTree
get id =
  first (\i tree -> 
    if i == id then 
      Just tree 
    else 
      Nothing
  )


updateFromTerms :
  Int 
  -> (SyntaxTree -> SyntaxTree) 
  -> List SyntaxTree 
  -> List SyntaxTree
updateFromTerms i fn terms =
  case terms of 
    [] ->
      [] 
    term :: rest ->
      update i fn term :: updateFromTerms (i - term.size) fn rest


update : Int -> (SyntaxTree -> SyntaxTree) -> SyntaxTree -> SyntaxTree
update i fn tree =
  case compare i tree.size of
    LT -> updateTerms (updateFromTerms i fn) tree
    EQ -> fn tree
    GT -> tree


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
  -> SNode (List a) 
  -> (String -> a)
  -> Maybe (List a)
translate grammar {name, alt, terms} f =
  Grammar.get name alt grammar 
    `Maybe.andThen` (\alt -> match alt terms f)

