module Ash.Frame exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)

import Ash.Serializer exposing (Serializer)
import Ash.SyntaxTree as SyntaxTree exposing (SyntaxTree)
import Ash.Command as Command 

import Style exposing (..)

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
  = Replace String
  | Change
  | Delete
  | Focus Direction
  | SmartFocus Direction

type Response 
  = NoOp
  | SetBufferData Int SyntaxTree
  | SuggestBufferData Int (List (Int, SyntaxTree))
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
    updateBuffer f data = 
      let newBuffer = f frame.focus data 
      in (frame, SuggestBufferData frame.bufferId [newBuffer, (frame.focus, data)])
  in 
    case Array.get frame.bufferId buffers of
      Just {data, language} -> 
        case action of
          
          Replace str -> 
            case Debug.log "parse" <| Language.parse language str of
              Just ast -> 
                updateBuffer (Command.update <| always ast) data
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

          Delete -> 
            updateBuffer Command.delete data

          Change ->
            (frame, Fail "Not Implemented")

      Nothing -> 
        fail <| "No buffer " ++ toString frame.bufferId

view : { a |  buffers : Array Buffer } -> Frame -> Html msg
view {buffers} frame = 
  case Array.get frame.bufferId buffers of
    Just buffer -> 
      div [ class "frame" ] 
        [ div [ class "frame-content" ] 
          [ debug 
            { focus = frame.focus
            , grammar = (Language.getGrammar buffer.language)
            , data = buffer.data
            } 
          ]
        , div [ class "frame-line" ] 
          [ p [] [ text <| Language.getName buffer.language]
          ]
        ]
    Nothing -> 
      div [ class "frame frame-no-buffer" ] 
        [ text "No Buffer" ]

debug {data, grammar, focus} = 
  let 
    collector id tree =
      let focusCls = if focus == id then ["ash-focus"] else []
      in div 
        [ classes <| [ "ash-dnode"] ++ focusCls] <| 
        [ div 
          [ class "ash-dnode-header"]
          [ text (fst tree.kind ++ " : " ++ toString id)] 
        ] ++ (
          Maybe.withDefault [ text "?" ] 
            <| SyntaxTree.translate grammar tree (\str -> 
              div 
                [ class "ash-dnode-term"] 
                [ text str ]
            )
        )
  in div [ class "ash-debug-tree" ] [ SyntaxTree.collect collector data ]

