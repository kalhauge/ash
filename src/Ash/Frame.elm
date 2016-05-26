module Ash.Frame exposing 
  ( Frame
  , new

  , setFocus
  , updateFocus
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
    , serializer = Language.getDefaultSerializer language
    }

getBufferId : Frame -> Int
getBufferId (Frame {bufferId}) = bufferId

{- Actions & Handlers -} 

type Msg 
  = OnBufferWithFocus (Int -> Buffer.Msg)
  | SetFocus Int
  | SetSerializer String 
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
  in case msg of
    Focus dir -> 
      updateWithBuffer <| moveFocus dir 
    
    SmartFocus dir -> 
      updateWithBuffer <| moveSmartFocus dir 

    SetFocus focus -> 
      Update (setFocus focus frame)

    OnBufferWithFocus msg -> 
      UpdateBuffer bufferId (msg focus)

    SetSerializer str -> 
      updateWithBuffer <| setSerializer str
      

setSerializer : String -> Frame -> Buffer -> Frame
setSerializer str (Frame frame) buffer =
  let lang = Buffer.getLanguage buffer
  in Frame 
    { frame
    | serializer = Language.getSerializer str lang 
    }

{- Movement -}

type alias Direction = Movement.Direction

setFocus : Focus -> Frame -> Frame
setFocus focus =
  updateFocus <| always focus 

updateFocus : (Focus -> Focus) -> Frame -> Frame
updateFocus f (Frame frame) = 
  Frame { frame | focus = f frame.focus } 

moveSmartFocus : Direction -> Frame -> Buffer -> Frame
moveSmartFocus dir (Frame {focus} as frame) =
  Buffer.onData (Movement.moveSmartFocus dir focus)
  >> flip setFocus frame

moveFocus : Direction -> Frame -> Buffer -> Frame
moveFocus dir (Frame {focus} as frame) =
  Buffer.onData (Movement.moveFocus dir focus)
  >> flip setFocus frame

{- Visuals -}

view : Frame -> Buffer -> Html ()
view (Frame frame) (Buffer buffer) = 
  div [ class "frame" ] 
    [ div [ class "frame-content" ] 
      [ frame.serializer
        { focus = frame.focus
        , grammar = (Language.getGrammar buffer.language)
        , data = buffer.data
        } 
      ]
    , div [ class "frame-line" ] 
      [ p [] [ text <| Language.getName buffer.language]
      ]
    ]


