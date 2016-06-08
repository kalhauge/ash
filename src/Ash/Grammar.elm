module Ash.Grammar exposing 
  ( Grammar
  , Rule
  , Alternative
  , Term (..)

  , ClauseId
  , AlternativeId
  , SyntaxKind
  
  , grammar
  , rule

  , oneOf

  , getRule
  , get

  , isTransition
  , isLexical

  , clauseIds
  , subClauses

  , ClausePath
  
  , transitiveClauses
  , transitiveKinds

  , reachableClauses
  , reachableKinds
  , kinds
  
  , parens
  )

import Utils exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)
import Char
import Maybe
import String

type alias Grammar     = Dict String Rule
type alias Rule        = Array Alternative
type alias Alternative = List Term
type Term
  = Lex String
  | Ref ClauseId

type alias ClauseId      = String
type alias AlternativeId = Int

type alias SyntaxKind = (ClauseId, AlternativeId)

grammar : String -> List (String, Rule) -> Grammar
grammar empty list = 
  Dict.fromList 
    ( List.map 
      (\(str, rule) -> (str, Array.push ([Ref "empty"]) rule))
       list
      ++ [( "empty", rule [[ Lex empty ]] )]
    )

rule : List Alternative -> Rule
rule = Array.fromList

oneOf : String -> Rule
oneOf str = 
  String.toList str
  |> List.map (String.fromChar >> \x -> [ Lex x ])
  |> rule

getRule : ClauseId -> Grammar -> Maybe Rule
getRule = Dict.get 

get : SyntaxKind -> Grammar -> Maybe Alternative
get (clauseId, altId) grammar = 
  Dict.get clauseId grammar `Maybe.andThen` Array.get altId 

getClauseId : Term -> Maybe ClauseId
getClauseId term = 
  case term of
    Lex _ -> Nothing
    Ref cid -> Just cid

clauseIds : Alternative -> List ClauseId 
clauseIds = 
  List.map getClauseId 
  >> compress

isTransition : Alternative -> Bool
isTransition alt = 
  let len = List.length alt 
  in len == 1 && len == List.length (clauseIds alt)

isLexical : ClauseId -> Bool
isLexical = 
  String.toList >> List.head 
  >> Maybe.map Char.isLower 
  >> Maybe.withDefault False

subClauses : Grammar -> SyntaxKind -> List ClauseId
subClauses grammar sid = 
  get sid grammar 
  |> Maybe.map clauseIds 
  |> Maybe.withDefault []

{-
Transitive clauses is a list of clauses that can be reached with no
extra syntax. 
-}
transitiveClauses : Grammar -> ClauseId -> List ClauseId
transitiveClauses =
  transitiveClauses' Set.empty

transitiveClauses' : Set ClauseId -> Grammar -> ClauseId -> List ClauseId
transitiveClauses' visited grammar cid = 
  if cid `Set.member` visited then []
  else
    let visited' = Set.insert cid visited 
    in cid :: case getRule cid grammar of
        Just alts -> 
          List.concatMap (\alt -> 
            case clauseIds alt of 
              [subcid] -> transitiveClauses' visited' grammar subcid 
              _ -> [] 
           ) <| List.filter isTransition
             <| Array.toList alts 
        Nothing -> []


transitiveKinds : Grammar -> ClauseId -> List SyntaxKind
transitiveKinds grammar cid =
  transitiveClauses grammar cid
  |> List.concatMap (kinds grammar)

  
type alias ClausePath = (ClauseId, List SyntaxKind)

reachableClauses : Grammar -> ClauseId -> List ClausePath
reachableClauses grammar cid = 
  let 
    reachables visited cid depth = 
      if cid `Set.member` visited then [] 
      else 
        let visited' = Set.insert cid visited 
        in (cid, depth) :: case getRule cid grammar of
          Just alts -> 
            List.concatMap (\(i, alt) -> 
              case clauseIds alt of 
                [] -> [] 
                [subcid] -> 
                  if isTransition alt then
                    reachables visited' subcid depth
                  else
                    let depth' = (cid, i) :: depth in
                    reachables visited' subcid depth'
                _ -> [] 
             ) <| Array.toIndexedList alts 
          Nothing -> 
            []
  in reachables Set.empty cid []

{-
If the syntax kind is a paran, only one child, return the
clause allowed inside the parans and the kind itself and the 
-}
paren : Grammar -> SyntaxKind -> Maybe (ClauseId, SyntaxKind)
paren grammar kind = 
  get kind grammar `Maybe.andThen` \alt -> 
  if isTransition alt then
    Nothing
  else 
    case clauseIds alt of
      [subid] -> Just (subid, kind)
      _ -> Nothing

parens : Grammar -> ClauseId -> List (ClauseId, SyntaxKind)
parens grammar clause = 
  transitiveKinds grammar clause
  |> List.map (paren grammar)
  |> Utils.compress

{-
Returns a list of syntax kinds directly reachable from the clause.
-}
kinds : Grammar -> ClauseId -> List SyntaxKind
kinds grammar cid = 
  getRule cid grammar 
  |> Maybe.map (List.map (\(i, _) -> (cid, i)) << Array.toIndexedList)
  |> Maybe.withDefault [] 

reachableKinds : Grammar -> ClauseId -> List (SyntaxKind, List SyntaxKind)
reachableKinds grammar cid =
  let
    helper (cid, cp) = 
      List.map (\kind -> (kind, cp)) 
      <| kinds grammar cid
  in List.concatMap helper <| reachableClauses grammar cid
