module Ash.Frame exposing 
  ( Frame
  , new

  , setFocus
  , getBufferId

  , moveSmartFocus
  , moveFocus
  
  , view

  , Msg(..)
  , Response(..)
  , update
  )

import Ash.Buffer as Buffer exposing (Buffer(..))
import Ash.Command as Command 
import Ash.Movement as Movement
import Ash.Language as Language exposing (Language)
import Ash.Serializer exposing (Serializer)
import Ash.SyntaxTree as SyntaxTree exposing (SyntaxTree, Focus)

import Array exposing (Array)
import Style exposing (..)
import Utils exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)

type Frame = Frame
  { serializer : Serializer 
  , focus : Int
  , bufferId : Int
  }

new : (Int, Buffer) -> Frame
new (i, Buffer {data, language}) = 
  Frame
    { focus = data.size 
    , bufferId = i 
    , serializer = Language.getDefaultSerialzier language
    }

getBufferId : Frame -> Int
getBufferId (Frame {bufferId}) = bufferId

{- Actions & Handlers -} 

type Msg 
  = Replace String
  | Change
  | Delete
  | Focus Direction
  | SmartFocus Direction

type Response
  = Update Frame
  | UpdateWithBuffer Int (Buffer -> Frame)
  | UpdateBuffer Int (Buffer.Msg) 
  | Fail String

update : Msg -> Frame -> Response
update msg (Frame {focus, bufferId} as frame) = 
  let 
    updateWithBuffer fn = 
      UpdateWithBuffer bufferId (fn frame)
    updateBuffer fn = 
      UpdateBuffer bufferId (fn focus)
  in case msg of
    Focus dir -> 
      updateWithBuffer <| moveFocus dir 
    
    SmartFocus dir -> 
      updateWithBuffer <| moveFocus dir 

    Delete -> 
      updateBuffer <| Buffer.Delete
    
    _ -> Fail "Not implemented"

{- Movement -}

type alias Direction = Movement.Direction

setFocus : Focus -> Frame -> Frame
setFocus focus (Frame frame) =
  Frame { frame | focus = focus } 

moveSmartFocus : Direction -> Frame -> Buffer -> Frame
moveSmartFocus dir (Frame {focus} as frame) =
  Buffer.onData (Movement.moveSmartFocus dir focus)
  >> flip setFocus frame

moveFocus : Direction -> Frame -> Buffer -> Frame
moveFocus dir (Frame {focus} as frame) =
  Buffer.onData (Movement.moveFocus dir focus)
  >> flip setFocus frame

{- Visuals -}

view : Frame -> Buffer -> Html msg
view (Frame frame) (Buffer buffer) = 
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
  in div [ class "ash-debug-tree" ] 
      [ SyntaxTree.collect collector data ]

