module Ash.Frame exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)

import Ash.Serializer exposing (Serializer)
import Ash.SyntaxTree as SyntaxTree exposing (SyntaxTree)
import Ash.Command as Command 

import Ash.Language as Language exposing (Language)

import Array exposing (Array)

type alias Buffer = 
  { data : SyntaxTree
  , language : Language
  }

type Direction 
  = Child
  | Parrent
  | Next
  | Prev

type Action 
  = Change String
  | Focus Direction
  | SmartFocus Direction

type Response 
  = NoOp
  | SetBufferData Int SyntaxTree
  | Fail String

type alias Frame = 
  { serializer : Serializer 
  , focus : Int
  , bufferId : Int
  }

update : { a | buffers : Array Buffer } -> Frame -> Action -> (Frame, Response)
update {buffers} frame action =
  let 
    fail msg = (frame, Fail msg)
  in 
    case Array.get frame.bufferId buffers of
      Just {data, language} -> 
        case action of
          
          Change str -> 
            case Debug.log "parse" <| Language.parse language str of
              Just ast -> 
                let 
                  (i, data') = Debug.log "update" <| 
                    Command.update (always ast) frame.focus data
                in 
                  ( { frame | focus = i }
                  , SetBufferData frame.bufferId data'
                  )
              Nothing -> 
                fail <| "Could not parse '" ++ str ++ "'"
          
          SmartFocus dir -> 
            let f = case dir of
              Child -> 
                Command.smartChild
              Parrent -> 
                Command.smartParrent
              Next -> 
                Command.smartNext
              Prev -> 
                Command.smartPrev
            in ({ frame | focus = f data frame.focus }, NoOp)

          Focus dir -> 
            let f = case dir of
              Child -> 
                Command.child
              Parrent -> 
                Command.parrent
              Next -> 
                Command.next
              Prev -> 
                Command.prev
            in ({ frame | focus = f data frame.focus }, NoOp)

      Nothing -> 
        fail <| "No buffer " ++ toString frame.bufferId

view : { a |  buffers : Array Buffer } -> Frame -> Html msg
view {buffers} frame = 
  case Array.get frame.bufferId buffers of
    Just buffer -> 
      div [ class "frame" ] 
        [ debug 
            { focus = frame.focus
            , grammar = (Language.getGrammar buffer.language)
            , data = buffer.data
            } 
        ]
    Nothing -> 
      div [ class "frame frame-no-buffer" ] 
        [ text "No Buffer" ]

debug {data, grammar, focus} = 
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
            <| SyntaxTree.translate grammar tree (\str -> 
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
  in SyntaxTree.collect collector data

