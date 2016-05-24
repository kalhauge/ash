module Ash.SyntaxTree exposing 
  (..)

import List exposing (sum, map2)

import Ash.Grammar as Grammar exposing (..)
import Utils exposing (..)

type alias SNode a
  = { kind : SyntaxKind
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


mapFromS : (List SyntaxTree -> List b) -> SyntaxTree -> SNode b
mapFromS fn a =
  { a | terms = fn (getTerms a) }


mapToS : (List a -> List SyntaxTree) -> SNode a -> SyntaxTree
mapToS fn a =
  setTerms (fn a.terms) a


setTerms : List SyntaxTree -> SNode a -> SyntaxTree
setTerms terms inner =
  syntax (inner.kind) terms


syntax : 
  SyntaxKind
  -> List SyntaxTree 
  -> SyntaxTree
syntax kind terms =
  { kind = kind
  , terms = List.map SubTree terms
  , size = sum (List.map .size terms) + 1
  }

setTermsS : List SubTree -> SNode b -> SubTree
setTermsS terms inner = 
  SubTree <| syntax inner.kind (List.map unfix terms)



empty = syntax ("empty", 0) []

-- Special functions from here

type alias Focus = Int

subIndecies : Int -> SyntaxTree -> List Int
subIndecies id tree =
  indecies (id - tree.size) (getTerms tree)

subWithIndecies : Int -> SyntaxTree -> List (Int, SyntaxTree)
subWithIndecies id tree =
  map2 (,) (subIndecies id tree) (getTerms tree)

clausesWithIndicies : Grammar -> Int -> SyntaxTree ->  List (Int, ClauseId) 
clausesWithIndicies grammar id st =
  Grammar.subClauses grammar st.kind
  |> List.map2 (,) (subIndecies id st)
  
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
      clt (i + tree.size) (mapFromS (step i) tree)
  in 
    iterate 0 

collectS : TreeCollector SubTree -> SyntaxTree -> SyntaxTree
collectS clt =
  collect clt >> unfix


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
      clt (i + tree.size) (mapFromS (step i) tree)
  in 
    iterate 0 



{- 
Finds the clause of the a tree in focus. To work with the trim function, and 
empty nodes.
-}
clause : Grammar -> Int -> SyntaxTree -> Maybe ClauseId
clause grammar id =
  let
    findClause i st = 
      clausesWithIndicies grammar i st
      |> Utils.oneOfMap (\(i, s) -> if i == id then Just s else Nothing)
  in search id findClause 


-- Should be put some where else.

match : 
  List a 
  -> (String -> a)
  -> Alternative
  -> Maybe (List a)
match terms f template =
  case (template, terms) of
    (Ref name :: template', term:: terms') ->
        match terms' f template' 
         |> tryPrepend term
    
    (Lex str :: template', _) ->
        match terms f template' 
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
translate grammar {kind, terms} f =
  let alt = Grammar.get kind grammar 
  in alt `Maybe.andThen` match terms f

