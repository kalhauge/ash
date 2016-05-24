module Ash.Language exposing (..)

import Dict exposing (Dict)

import Ash.Grammar as Grammar exposing (..)

import Ash.Serializer as Serializer exposing (Serializer, default)
import Ash.Parser as Parser 
import Ash.SyntaxTree as SyntaxTree exposing (SyntaxTree)
import Ash.Command as Command

type Language = Language
  { name : String
  , grammar : Grammar
  , headExpr : ClauseId
  }

collect : List Language -> Dict String Language
collect = 
  List.map (\(Language {name} as l) -> (name, l)) >> Dict.fromList

getDefaultSerialzier : Language -> Serializer
getDefaultSerialzier language = default

parse : Language -> ClauseId -> String -> Maybe SyntaxTree
parse (Language language) cid = 
  Parser.parse (language.grammar) cid
  >> Maybe.map (Command.trim language.grammar)

getGrammar : Language -> Grammar
getGrammar (Language {grammar}) = grammar

getName : Language -> String
getName (Language {name}) = name

clause : Language -> Int -> SyntaxTree -> ClauseId
clause (Language {headExpr, grammar}) focus st = 
  Maybe.withDefault headExpr
    <| SyntaxTree.clause grammar focus st

reacableClauses : Language -> ClauseId -> List Grammar.ClausePath
reacableClauses (Language {grammar} as l) clause = 
  Grammar.reachableClauses grammar clause 


