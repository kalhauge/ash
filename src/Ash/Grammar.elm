module Ash.Grammar exposing 
  ( Grammar
  , Rule
  , Alternative
  , Term (..)

  , ClauseId
  , AlternativeId
  , SyntaxId
  
  , grammar
  , rule

  , oneOf

  , getRule
  , get

  , clauseIds
  , subClauses

  , ClausePath
  , reachableClauses
  )

import Utils exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)
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

type alias SyntaxId = (ClauseId, AlternativeId)

grammar : List (String, Rule) -> Grammar
grammar = Dict.fromList

rule : List Alternative -> Rule
rule = Array.fromList

oneOf : String -> Rule
oneOf str = 
  String.toList str
  |> List.map (String.fromChar >> \x -> [ Lex x ])
  |> rule

getRule : ClauseId -> Grammar -> Maybe Rule
getRule = Dict.get 

get : SyntaxId -> Grammar -> Maybe Alternative
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


subClauses : Grammar -> SyntaxId -> List ClauseId
subClauses grammar sid = 
  get sid grammar 
  |> Maybe.map clauseIds 
  |> Maybe.withDefault []
  
type alias ClausePath = (ClauseId, List SyntaxId)

{-
Return a list of clause paths, a clause path is a clauseid and 
a list of syntaxid's needed to create it.
-}
reachableClauses : Grammar -> ClauseId -> List ClausePath
reachableClauses grammar cid = 
  let 
    reachables visited cid depth = 
      if cid `Set.member` visited 
         then [] 
      else 
        let visited' = Set.insert cid visited 
        in case getRule cid grammar of
          Just alts -> 
            List.concatMap (\(i, alt) -> 
              case clauseIds alt of 
                [] -> [] 
                [subcid] -> 
                  case alt of
                    [] -> [] 
                    [a] -> 
                      reachables visited' subcid depth
                    _ -> 
                      let depth' = (cid, i) :: depth in
                      (subcid, depth') :: reachables visited' subcid depth'
                _ -> [] 
             ) <| Array.toIndexedList alts 
          Nothing -> 
            []
  in [(cid, [])] ++ reachables Set.empty cid []
  
  -- let reachableClauses' visited cid = 
  --   case getRule cid grammar of
  --     Just array -> 
  --       let 
  --         subs = 
  --           Set.fromList <| List.concatMap clauseIds (Array.toList array)
  --           
  --         unvisited = 
  --           Set.diff visited subs

  --         visited' = 
  --           Set.union visited subs
  --           
  --         visitor = 
  --           reachableClauses' visited'

  --       in 
  --         List.concatMap visitor (Set.toList <| unvisited)
  --         |> List.map (prepend cid) 
  --     Nothing -> []
  -- in reachableClauses' (Set.singleton cid) cid
