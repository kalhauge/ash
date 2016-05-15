module Ash.Serializer exposing (Serializer)

import Html exposing (..)

import Ash.Grammar exposing (Grammar)
import Ash.SyntaxTree exposing (SyntaxTree)

type alias Serializer = 
  { data: SyntaxTree, grammar : Grammar } 
  -> Html () 
