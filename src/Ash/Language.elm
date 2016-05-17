module Ash.Language exposing (..)

import Dict exposing (Dict)

import Ash.Grammar as Grammar exposing (..)

import Ash.Serializer as Serializer exposing (Serializer, default)
import Ash.Parser as Parser 
import Ash.SyntaxTree as SyntaxTree exposing (SyntaxTree)

type Language = Language
  { name : String
  , grammar : Grammar
  , headExpr : String
  }

collect : List Language -> Dict String Language
collect = 
  List.map (\(Language {name} as l) -> (name, l)) >> Dict.fromList

getDefaultSerialzier : Language -> Serializer
getDefaultSerialzier language = default

parse : Language -> String -> Maybe SyntaxTree
parse (Language language) = 
  Parser.parse (language.grammar) (language.headExpr)

getGrammar : Language -> Grammar
getGrammar (Language {grammar}) = grammar
