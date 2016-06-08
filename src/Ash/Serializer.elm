module Ash.Serializer exposing (
  Serializer, 
  simple, debug,

  Encoding (..),
  encode
  )

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

type Encoding
  = Token String
  | Syntax Int (List Encoding)
  | Lexical Int (List Encoding)
  | Empty
  | Error
  | Group Int (List Encoding)

encode : (SyntaxTree -> Encoding) -> Serializer
encode encoder {data, grammar, focus} =
  let 
    encoding = 
      encoder data

    joinTokens tokens = 
      List.intersperse (text " ") tokens

    printer encoding = 
      case encoding of 
        Token str -> span [] [ text str ]
        Error -> 
          span [] [ text "!" ]
        Empty -> 
          span [] [ text "?" ]
        Syntax i list -> 
          div (if i == focus then [class "focus"] else []) 
            <| joinTokens <| List.map printer  list
        Lexical i list -> 
          div (if i == focus then [class "focus"] else []) 
            <| List.map printer list
        Group i list ->
          div [ class "group"
              , style [ ("padding-left", toString i ++ "em") ]] 
            <| joinTokens <| List.map printer list
  in 
    div [ class "ash-encoding" ] [ printer encoding ]

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
