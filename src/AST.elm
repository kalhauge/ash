module AST where

import Array
import Dict
import Maybe
import String

type alias Grammar     = Dict.Dict String Rule
type alias Rule        = Array.Array Alternative
type alias Alternative = List Term
type Term
  = Lex String
  | Ref String

grammar : List (String, Rule) -> Grammar
grammar = Dict.fromList

rule : List Alternative -> Rule
rule = Array.fromList

oneOf : String -> Rule
oneOf str = 
  String.toList str
  |> List.map (String.fromChar >> \x -> [ Lex x ])
  |> rule

type alias ClauseId      = String
type alias AlternativeId = Int

type SyntaxTree 
  = SyntaxTree
      { name : ClauseId
      , alt : AlternativeId 
      , terms : List SyntaxTree
      , focus : Bool
      }

getType : SyntaxTree -> (ClauseId, AlternativeId) 
getType (SyntaxTree {name, alt}) =
  (name, alt)


setTerms : List SyntaxTree -> SyntaxTree -> SyntaxTree
setTerms terms (SyntaxTree inner) =
  SyntaxTree { inner | terms = terms }

updateTerms : 
  (List SyntaxTree -> List SyntaxTree) 
  -> SyntaxTree 
  -> SyntaxTree
updateTerms fn (SyntaxTree inner) =
  SyntaxTree { inner | terms = fn inner.terms }

setFocus : Bool -> SyntaxTree -> SyntaxTree
setFocus focus (SyntaxTree inner) = 
  SyntaxTree { inner | focus = focus }

hasFocus : SyntaxTree -> Bool
hasFocus (SyntaxTree inner) =
  inner.focus

syntax : 
  ClauseId 
  -> AlternativeId
  -> Bool 
  -> List SyntaxTree 
  -> SyntaxTree
syntax name alt focus terms =
  SyntaxTree
    { name = name
    , alt = alt
    , terms = terms
    , focus = focus
    }


tryPrepend : a -> Maybe (List a) -> Maybe (List a)
tryPrepend a ls = 
  ls `Maybe.andThen` (\ls' -> Just <| a :: ls')


match : Alternative
  -> List SyntaxTree 
  -> (SyntaxTree -> a)
  -> (String -> a)
  -> Maybe (List a)
match ts sts f g =
  case (ts, sts) of
    (Ref name :: ts', (SyntaxTree st as st') :: sts') ->
      --if name == st.name then
        tryPrepend (f st') <| match ts' sts' f g
      --else 
      --  Nothing
    (Lex str :: ts', _) ->
      tryPrepend (g str) <| match ts' sts f g
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
translate grammar (SyntaxTree {terms} as st) f g =
  getSyntax st grammar `Maybe.andThen` (\alt -> match alt terms f g)


get : ClauseId -> AlternativeId -> Grammar -> Maybe Alternative
get clauseId altId grammar = 
  Dict.get clauseId grammar `Maybe.andThen` Array.get altId 


getSyntax : SyntaxTree -> Grammar -> Maybe Alternative
getSyntax (SyntaxTree tree) =
  get tree.name tree.alt 


tryUpdate : List a -> (a -> Maybe a) -> Maybe (List a)
tryUpdate al f = 
  let al' = List.map f al
  in case Maybe.oneOf al' of 
    Just _ -> Just <| List.map2 Maybe.withDefault al al'
    Nothing -> Nothing

