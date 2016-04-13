module AST where

import List exposing (sum, map)

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

getTerms : SyntaxTree -> List SyntaxTree 
getTerms {terms} = 
  unFix terms


setTerms : List SyntaxTree -> SyntaxTree -> SyntaxTree
setTerms terms inner =
  { inner | terms = SFix terms }


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
  -- Also calculate the size
  , size = sum (map .size terms)
  }

-- Special functions from here

get : Int -> SyntaxTree -> Maybe SyntaxTree
get i tree =
  case compare i tree.size of
    LT -> getFromTerms i(getTerms tree)
    EQ -> Just tree
    GT -> Nothing


getFromTerms : Int -> List SyntaxTree -> Maybe SyntaxTree
getFromTerms i terms =
  case terms of 
    [] ->
      Nothing
    term :: rest ->
      case get i term of
        Nothing -> getFromTerms (i - term.size) rest
        a -> a 


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


match : Alternative
  -> List SyntaxTree 
  -> (SyntaxTree -> a)
  -> (String -> a)
  -> Maybe (List a)
match template terms f g =
  case (template, terms) of
    (Ref name :: template', term:: terms') ->
        match template' terms' f g
         |> tryPrepend (f term) 
    
    (Lex str :: template', _) ->
        match template' terms f g
         |> tryPrepend (g str) 
    
    ([], []) ->
      Just []
    
    _ ->
      Nothing

translate : 
  Grammar 
  -> SyntaxTree 
  -> (SyntaxTree -> a)
  -> (String -> a)
  -> Maybe (List a)
translate grammar {name, alt, terms} f g =
  Grammar.get name alt grammar `Maybe.andThen` (\alt -> match alt (unFix terms) f g)

