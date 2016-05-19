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

import Char
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
  , keymaps : List (Char,String)
  }

{-
The main data structure.
-}
type alias Editor =
  { buffers : Array Buffer
  , frame : Maybe Frame  
  -- this variable should also contain somthing about the layout 
  , mode : Mode
  , languages : Dict String Language
  , keymaps : Dict Char.KeyCode KeyAction 
  }

type KeyAction = KeyAction (Editor -> Msg)

model : Settings -> Editor
model settings = 
  { buffers = fromList []
  , frame = Nothing 
  , mode = Normal
  , languages = Language.collect settings.languages
  , keymaps = Dict.fromList 
      <| List.map 
        (\(fst,snd) -> (Char.toCode fst, KeyAction <| parseMsg snd)) 
        settings.keymaps
  }

type Msg
  = NoOp 
  | NewBuffer Language
  | DoFrame Frame.Action
  | Fail String 
  | WithInput String (String -> Editor -> Msg)
  | SetMode Mode
  | DoAll (List Msg)


doMsg msg model = 
  let 
    fail msg =
      { model | mode = Failed msg model.mode }
  in case msg of
    DoAll actions -> 
      List.foldl doMsg model actions
    
    NoOp -> model
    
    Fail msg -> 
      fail msg
    
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
                fail msg 
          in { model'' | frame = Just frame }
        Nothing ->
          fail "No active frame" 

    SetMode mode -> 
      { model | mode = mode }

    WithInput pre f -> 
      { model | mode = Command pre (\str -> DoAll [SetMode Normal, f str model]) }

update : Msg -> Editor -> (Editor, Cmd Msg)
update msg model = 
  doMsg msg model ! []


parseMsg : String -> Editor -> Msg
parseMsg str model =
  let 
    args = 
      String.split " " <| String.trim str
    
    badArgs = 
      Fail "Bad number of arguments."

    parseDir str err f = 
      case str of
        "child" -> f Frame.Child
        "parrent" -> f Frame.Parrent
        "next" -> f Frame.Next
        "prev" -> f Frame.Prev
        _ -> err 

    parseArgs args = 
      case args of 
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

            "replace" -> 
              case rest of
                [expr] -> 
                  DoFrame (Frame.Replace expr)
                _ -> badArgs 
            
            "delete" -> 
                DoFrame (Frame.Delete)
            
            "change" -> 
                DoFrame (Frame.Change)

            "focus" -> 
              case rest of 
                [dir] -> 
                  parseDir dir
                    (Fail <| "Could not parse direction '" ++ dir ++ "'") 
                    (DoFrame << Frame.Focus)
                _ -> badArgs 
            
            "focus!" -> 
              case rest of
                [dir] -> 
                  parseDir dir
                    (Fail <| "Could not parse direction '" ++ dir ++ "'") 
                    (DoFrame << Frame.SmartFocus)
                _ -> badArgs 

            "withInput" -> 
              case rest of
                pre :: rest -> 
                  WithInput pre (\i -> parseMsg (String.join " " rest ++ " " ++ i))
                _ -> badArgs
            
            "command" ->
              parseArgs rest 
            
            _ -> 
              Fail <| "Unknown command '" ++ cmd ++ "'"

        [] -> NoOp

  in Debug.log "Msg" <| parseArgs args

type Mode
  = Normal 
  | Failed String Mode
  | Command String (String -> Msg)
  -- | Choose (Int, (Array (Int, SyntaxTree)))

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
              [ text "Welcome to Ash. To continue use :new <language>." ] 
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
    Normal -> Nothing
    
    Failed msg mode -> 
      Just <|
        div [ class "cmdline cmdline-failed"]
          [ p [] [text ("Failed: " ++ msg)] ]
    
    Command pre f -> 
      Just <| 
        div [ class "cmdline" ] 
          [ p [] [text pre]
          , input 
            [ onEnter f
            , autofocus True
            , attribute "data-autofocus" ""
            ] [] 
          ]
    

keyHandler : Editor -> Keyboard.KeyCode -> Msg
keyHandler editor key =
  --let key' = Debug.log "key" (Char.fromCode key) in 
  case editor.mode of
    Normal -> 
      case key of
        a  -> 
          Dict.get a editor.keymaps
          |> Maybe.map (\(KeyAction f) -> f editor)
          |> Maybe.withDefault NoOp 
    
    Failed msg mode-> 
      DoAll 
        [ SetMode mode
        , keyHandler { editor | mode = mode } key
        ]

    Command pre f ->
      case key of
        96 -> SetMode Normal
        _ -> NoOp

specialKeyHandler : Editor -> Keyboard.KeyCode -> Msg
specialKeyHandler editor key =
  --let key' = Debug.log "specialkey" key in 
  case editor.mode of
    Normal -> NoOp
    Failed msg mode -> NoOp
    Command pre f -> 
      case key of
        27 -> SetMode Normal
        _ -> NoOp

subscriptions editor = 
  Sub.batch 
    [ Keyboard.presses (keyHandler editor)
    , Keyboard.ups (specialKeyHandler editor)
    ]

editor : Settings -> Program Never
editor settings = 
  program 
    { init = ( model settings, settings.after )
    , subscriptions = subscriptions
    , update = update 
    , view = view 
    }
