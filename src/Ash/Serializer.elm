module Ash.Serializer exposing (Serializer, Mode(..), dpprint, pprint)

import Html exposing (..)
import Html.Attributes exposing (..)

import Array

import Ash.Grammar exposing (Grammar)
import Ash.SyntaxTree exposing (SyntaxTree, collect, translate)

type Mode 
  = Normal
  | Change String
  | Choose (Int, (Array.Array (Int, SyntaxTree)))

type alias ViewT = 
  { data : SyntaxTree
  , grammar : Grammar
  , focus : Int 
  , mode : Mode
  } 

type alias Serializer = 
  ViewT
  -> Html () 

dpprint : Serializer 
dpprint {data, grammar, focus} = 
  let 
    collector id tree =       
      div 
        [ style <|
          [ ("display", "inline-block")
          , ("margin", "2px 2px 0px 2px")
          , ("text-align", "center")
          ] ++ if focus == id then 
            [ ("background", "lightgray") ]
          else []
        ]
        <| [ div 
          [ style <|
            [ ("font-size", "6pt") ] ++ 
            if focus == id then 
              [ ("background", "black") 
              , ("color", "lightblue") 
              ]
            else 
              [("background", "lightblue")]
          ] 
          [ text (fst tree.kind ++ " : " ++ toString id)] 
        ] ++ (
          Maybe.withDefault [ text "?" ] 
            <| translate grammar tree (\str -> 
              div 
                [ style <|
                  [ ("display", "inline-block")
                  , ("margin", "2px 2px 0px 2px")
                  , ("text-align", "center")
                  ] ++ if focus == id then 
                    [ ("color", "red") ]
                  else []
                ] [ text str ]
            )
        )
  in collect collector data

pprint : Serializer 
pprint {data, grammar, focus, mode} =
  let
    collector id tree =
      div 
        [ style <| 
          [ ("display", "inline") ] 
            ++ if id == focus then
              [ ("background", "lightgray") ]
            else []
        ]
        ( Maybe.withDefault 
            [ case mode of 
                Change str -> text str
                _ -> text "?"
            ] 
            <| translate grammar tree (\str -> 
                  div  
                    [ style <| 
                      [ ("display", "inline") ] 
                        ++ if id == focus then
                          [ ("color", "red") ]
                        else []
                    ]
                    [ text str ]
                )
        )
  in
     collect collector data
