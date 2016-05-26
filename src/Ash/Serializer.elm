module Ash.Serializer exposing (Serializer, simple, debug)

import Html exposing (..)
import Html.Attributes exposing (..)

import Array

import Ash.Grammar as Grammar exposing (Grammar)
import Ash.SyntaxTree as SyntaxTree exposing (SyntaxTree, collect, translate)

import Style exposing (classes, attr)

type alias ViewT = 
  { data : SyntaxTree
  , grammar : Grammar
  , focus : Int 
  } 

type alias Serializer = 
  ViewT
  -> Html () 

debug : Serializer
debug {data, grammar, focus} = 
  let 
    collector id tree =
      let focusCls = if focus == id then ["ash-focus"] else []
      in div 
        [ classes <| [ "ash-dnode"] ++ focusCls] <| 
        [ div 
          [ class "ash-dnode-header"]
          [ text (fst tree.kind ++ "/" ++ toString id )] 
        ] ++ (
          Maybe.withDefault [ text "?" ] 
            <| SyntaxTree.translate grammar tree (\str -> 
              div 
                [ class "ash-dnode-term"] 
                [ text str ]
            )
        )
  in div [ class "ash-debug-tree" ] 
      [ SyntaxTree.collect collector data ]

simple : Serializer
simple {data, grammar, focus} =
  let 
    collector id tree =
      let focusCls = if focus == id then ["ash-focus"] else []
      in div 
        [ classes <| [ "ash-snode"] ++ focusCls] 
        <| Maybe.withDefault [ text "?" ] 
        <| SyntaxTree.translate grammar tree (\str -> 
              div 
                [ class "ash-snode-term"] 
                [ text str ]
            )
  in div [ class "ash-simple-tree" ] 
      [ SyntaxTree.collect collector data ]
