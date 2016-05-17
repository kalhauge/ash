module Ash.Editor exposing (..)

{- 
This module contains the main data structure for controling the editor.
-}

import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (class, autofocus, style, attribute)
import Html.Events exposing (onInput, on)


import Json.Decode as Json

import Platform exposing (Program)
import Platform.Cmd exposing (Cmd(..), none, (!))

import Keyboard exposing (presses)

import String 
import Array exposing (Array, fromList)
import Dict exposing (Dict)

import Utils exposing (..)

import Ash.SyntaxTree as SyntaxTree exposing (SyntaxTree)
import Ash.Grammar exposing (Grammar)
import Ash.Language as Language exposing (Language)

import Ash.Frame as Frame exposing (Frame, Buffer)

type alias Settings = 
  { after : Cmd Msg
  , languages : List Language
  }

{-
The main data structure.
-}
type alias Editor =
  { buffers : Array Buffer
  , frame : Maybe Frame  
  -- this variable should also contain somthing about the layout 
  , mode : EditorMode
  , languages : Dict String Language
  }

model = 
  { buffers = fromList []
  , frame = Nothing 
  , mode = Passthrough
  , languages = Dict.empty
  }

type EditorAction
  = NoOp 
  | NewBuffer Language
  | DoFrame Frame.Action
  | Fail String

doAction : EditorAction -> Editor -> Editor
doAction action model = 
  case action of
    NoOp -> model
    
    Fail msg -> 
      { model | mode = Failed msg }
    
    NewBuffer language -> 
      let
        buffer = 
          { data = SyntaxTree.empty
          , language = language 
          }       
        buffers = Array.push buffer model.buffers 
        frame = 
          { focus = 1
          , bufferId = Array.length buffers - 1
          , mode = Frame.NormalMode
          , serializer = Language.getDefaultSerialzier language
          }
      in 
        { model | 
            buffers = buffers, 
            frame = Just frame
        }

    DoFrame action ->
      case model.frame of
        Just frame -> 
          let 
            (frame, response) = Debug.log "frame" <| Frame.update model frame action 
            model'' = case response of
              Frame.NoOp -> 
                model
              Frame.SetBufferData i data -> 
                { model
                | buffers = 
                    Maybe.withDefault model.buffers
                      <| arrayUpdate i
                          (\x -> {x | data = data }) 
                          model.buffers
                }
              Frame.Fail msg -> 
                { model | mode = Failed msg }
          in { model'' | frame = Just frame }
        Nothing ->
          { model | mode = Failed "No active frame" }


parseAction : String -> Editor -> EditorAction
parseAction str model =
  let 
    args = String.split " " <| String.trim str
    badArgs = 
      Fail "Bad number of arguments, takes one; the name of the language"
  in Debug.log "Action" <| case args of 
    cmd :: rest -> 
      case cmd of
        
        "new" -> 
          case rest of
            [name] -> 
              case Dict.get name model.languages of
                Just language -> 
                  NewBuffer language
                Nothing -> 
                  Fail <| "Language '" ++ name ++ "' not known to Ash"
            _ -> badArgs

        "change" -> 
          case rest of
            [expr] -> 
              DoFrame (Frame.Change expr)
            _ -> badArgs 

        _ -> 
          Fail <| "Unknown command '" ++ cmd ++ "'"

    [] -> NoOp

type EditorMode
  = Passthrough 
  | Command 
  | Failed String

type Msg 
  = SetMode EditorMode
  | Do EditorAction
  | ParseCmd String

update : Msg -> Editor -> (Editor, Cmd Msg)
update msg model = 
  let model' = case msg of
    SetMode mode -> 
      { model | mode = mode }
    ParseCmd str -> 
      doAction 
        (parseAction str model) 
        { model | mode = Passthrough }
    Do action -> 
      doAction action model
  in model' ! []


view : Editor -> Html Msg 
view editor = 
  div [ class "ash" ] <| 
    [ div [ class "editor" ] 
      [ 
        case editor.frame of
          Just frame -> 
            Frame.view editor frame
          Nothing -> 
            div [ class "frame no-frame" ] 
              [ text "No Frame" ] 
      ]
    ] 
    ++ maybeToList (cmdline editor.mode)

onEnter : (String -> msg) -> Attribute msg
onEnter tagger = 
  let 
    onlyOnEnter = 
      Json.object2 (,) 
        Html.Events.keyCode 
        Html.Events.targetValue
      `Json.andThen` (\(a,b) -> 
        if a == 13 then 
           Json.succeed b 
        else 
           Json.fail "only fire on enter"
      )
  in on "keyup" (Json.map tagger onlyOnEnter)

cmdline mode = 
  case mode of
    Passthrough -> Nothing
    Command -> 
      Just <| 
        div [ class "cmdline" ] 
          [ p [] [text ":"]
          , input 
            [ onEnter ParseCmd 
            , autofocus True
            , attribute "data-autofocus" ""
            ] [] 
          ]
    Failed msg -> 
      Just <|
        div [ class "cmdline cmdline-failed"]
          [ p [] [text ("Failed: " ++ msg)] ]

keyhandler mode key =
  case mode of
    Passthrough -> 
      case key of
        -- :
        58 -> SetMode Command 
        a  -> Do NoOp 
    Command -> 
      Do NoOp
    Failed msg -> 
      SetMode Passthrough

subscriptions editor = 
  presses (keyhandler editor.mode)

editor : Settings -> Program Never
editor settings = 
  program 
    { init = ( 
      { model | 
        languages = Language.collect settings.languages
      } 
      , settings.after
    )
    , subscriptions = subscriptions
    , update = update 
    , view = view 
    }
