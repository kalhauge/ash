module Ash.Language exposing (..)

import Dict exposing (Dict)

import Lazy.List as LazyList exposing (LazyList)

import Ash.Grammar as Grammar exposing (..)

import Ash.Serializer as Serializer exposing (Serializer, debug, simple)
import Ash.Parser as Parser 
import Ash.SyntaxTree as SyntaxTree exposing (SyntaxTree)
import Ash.Command as Command

type Language = Language
  { name : String
  , grammar : Grammar
  , headExpr : ClauseId
  , serializers : Dict String Serializer
  , defaultSerializer : Serializer
  }

new b = 
  let serializers = Dict.fromList ([("debug", debug), ("simple", simple)] ++ b.serializers)
  in Language 
      { b |
        serializers = serializers,
        defaultSerializer = getSerializer' (b.defaultSerializer) simple serializers
      }

collect : List Language -> Dict String Language
collect = 
  List.map (\(Language {name} as l) -> (name, l)) >> Dict.fromList

getDefaultSerializer : Language -> Serializer
getDefaultSerializer (Language language) = language.defaultSerializer 

getSerializer' : String -> Serializer ->  Dict String Serializer -> Serializer 
getSerializer' str default serializers =
  Maybe.withDefault default (Dict.get str serializers)

getSerializer : String -> Language -> Serializer
getSerializer str (Language {serializers} as lang) =
  getSerializer' str (getDefaultSerializer lang) serializers


parse : Language -> ClauseId -> String -> Maybe SyntaxTree
parse (Language language) cid = 
  Parser.parse (language.grammar) cid
  >> Maybe.map (Command.trim language.grammar)

suggest : Language -> ClauseId -> String -> LazyList SyntaxTree
suggest (Language language) cid = 
  Parser.suggest (language.grammar) cid
  >> LazyList.map (Command.trim language.grammar)

getGrammar : Language -> Grammar
getGrammar (Language {grammar}) = grammar

getName : Language -> String
getName (Language {name}) = name

clause : Language -> Int -> SyntaxTree -> ClauseId
clause (Language {headExpr, grammar}) focus st = 
  Maybe.withDefault headExpr
    <| SyntaxTree.clause grammar focus st

isValid : Language -> SyntaxTree -> Bool
isValid (Language {grammar, headExpr}) st =
  True

