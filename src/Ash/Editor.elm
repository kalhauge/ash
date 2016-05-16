module Ash.Editor exposing (..)

{- 
This module contains the main data structure for controling the editor.
-}

import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (class)

import Platform exposing (Program)
import Platform.Cmd exposing (Cmd, none)

import Keyboard exposing (presses)

import Array exposing (Array, fromList)

import Ash.SyntaxTree exposing (SyntaxTree)
import Ash.Grammar exposing (Grammar)

import Ash.Frame exposing (Frame)

type alias Buffer = 
  { data : SyntaxTree
  , grammar : Grammar
  }

type alias Settings = 
  { after : Cmd Msg
  }

{-
The main data structure.
-}
type Editor = Editor 
  { buffers : Array Buffer
  , frames : List Frame  
  -- this variable should also contain somthing about the layout 
  , mode : EditorMode
  }

model = Editor
  { buffers = fromList []
  , frames = []
  , mode = Passthrough
  }

type EditorMode
  = Passthrough 
  | Command String

type EditorAction
  = NoOp 

type Msg 
  = SetMode EditorMode
  | Do EditorAction

update : msg -> Editor -> (Editor, Cmd msg)
update msg model = 
  (model, none)

view (Editor editor) = 
  div [ class "ash" ] 
    (cmdline editor.mode)

cmdline mode = 
  case mode of
    Passthrough -> []
    Command a -> 
      [div [] []]

keyhandler mode key =
  case mode of
    Passthrough -> 
      case key of
        58 -> SetMode (Command "") -- :
        a  -> Do NoOp 
    Command s ->
      Do NoOp

subscriptions (Editor editor) = 
  presses (keyhandler editor.mode)

editor : Settings -> Program Never
editor settings = 
  program 
    { init = (model, settings.after)
    , subscriptions = subscriptions
    , update = update 
    , view = view 
    }
