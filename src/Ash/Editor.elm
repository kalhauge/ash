module Ash.Editor exposing (..)

{- 
This module contains the main data structure for controling the editor.
-}

import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (class, autofocus, style, attribute)
import Html.Events exposing (onInput, on)

import Style exposing (..)

import Json.Decode as Json

import Platform exposing (Program)
import Platform.Cmd exposing (Cmd(..), none, (!))

import Keyboard exposing (presses)

import Char
import String 
import Array exposing (Array, fromList)
import Dict exposing (Dict)

import Utils exposing (..)

import Ash.SyntaxTree as SyntaxTree exposing (SyntaxTree, SubTree(..), Focus)
import Ash.Grammar exposing (Grammar, Term(..))
import Ash.Language as Language exposing (Language(..))
import Ash.Movement as Movement

import Ash.Frame as Frame exposing (Frame)
import Ash.Buffer as Buffer exposing (Buffer)

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
  fix settings 
    { buffers = fromList []
    , frame = Nothing 
    , mode = Normal
    }

fix : 
  Settings 
  -> { buffers : Array Buffer, frame : Maybe Frame, mode : Mode } 
  -> Editor
fix settings model =
  { buffers = model.buffers
  , frame = model.frame
  , mode = model.mode
  , languages = Language.collect settings.languages
  , keymaps = Dict.fromList 
      <| List.map 
        (\(fst,snd) -> (Char.toCode fst, KeyAction <| parseMsg snd)) 
        settings.keymaps
  }

debug : Settings -> Editor
debug settings = 
  let buffer = 
    Buffer.Buffer { data = { kind = ("Exp",0), terms = [SubTree { kind = ("AddExp",0), terms = [SubTree { kind = ("AddExp",2), terms = [SubTree { kind = ("MulExp",2), terms = [SubTree { kind = ("ExpExp",1), terms = [SubTree { kind = ("PriExp",3), terms = [SubTree { kind = ("number",0), terms = [SubTree { kind = ("digit",1), terms = [], size = 1 },SubTree { kind = ("number",0), terms = [SubTree { kind = ("digit",2), terms = [], size = 1 },SubTree { kind = ("number",1), terms = [SubTree { kind = ("digit",3), terms = [], size = 1 }], size = 2 }], size = 4 }], size = 6 }], size = 7 }], size = 8 }], size = 9 }], size = 10 },SubTree { kind = ("MulExp",2), terms = [SubTree { kind = ("ExpExp",1), terms = [SubTree { kind = ("PriExp",3), terms = [SubTree { kind = ("number",0), terms = [SubTree { kind = ("digit",2), terms = [], size = 1 },SubTree { kind = ("number",1), terms = [SubTree { kind = ("digit",3), terms = [], size = 1 }], size = 2 }], size = 4 }], size = 5 }], size = 6 }], size = 7 }], size = 18 }], size = 19 }
    , language = Language { name = "math", grammar = Dict.fromList [("AddExp",Array.fromList [[Ref "AddExp",Lex "+",Ref "MulExp"],[Ref "AddExp",Lex "-",Ref "MulExp"],[Ref "MulExp"]]),("Exp",Array.fromList [[Ref "AddExp"]]),("ExpExp",Array.fromList [[Ref "PriExp",Lex "^",Ref "ExpExp"],[Ref "PriExp"]]),("MulExp",Array.fromList [[Ref "MulExp",Lex "*",Ref "ExpExp"],[Ref "MulExp",Lex "/",Ref "ExpExp"],[Ref "ExpExp"]]),("PriExp",Array.fromList [[Lex "(",Ref "Exp",Lex ")"],[Lex "-",Ref "PriExp"],[Ref "ident"],[Ref "number"]]),("alpha",Array.fromList [[Lex "a"],[Lex "b"],[Lex "c"],[Lex "d"],[Lex "e"],[Lex "f"],[Lex "g"],[Lex "h"],[Lex "i"],[Lex "j"],[Lex "k"],[Lex "l"],[Lex "m"],[Lex "n"],[Lex "o"],[Lex "p"],[Lex "q"],[Lex "r"],[Lex "s"],[Lex "t"],[Lex "u"],[Lex "v"],[Lex "x"],[Lex "y"],[Lex "z"]]),("digit",Array.fromList [[Lex "0"],[Lex "1"],[Lex "2"],[Lex "3"],[Lex "4"],[Lex "5"],[Lex "6"],[Lex "7"],[Lex "8"],[Lex "9"]]),("ident",Array.fromList [[Ref "alpha",Ref "ident"],[Ref "alpha"]]),("number",Array.fromList [[Ref "digit",Ref "number"],[Ref "digit"]])], headExpr = "Exp" } 
    }
  in fix settings 
    { buffers = Array.fromList [ buffer ]
    , frame = Just <| Frame.new (0, buffer) 
    , mode = Normal
    }

type Msg
  = NoOp 
  | ChooseMsg ChooseMsg
  | DoAll (List Msg)
  | DoFrame Frame.Msg
  | DoBuffer Int Buffer.Msg
  | Fail String 
  | NewBuffer Language
  | SetMode Mode
  | State
  | WithInput String (String -> Editor -> Msg)

type ChooseMsg 
  = Next
  | Prev
  | Done

realizeChoice : Editor -> Editor
realizeChoice editor = 
  case Debug.log "mode" <| editor.mode of  
    Choose i v arr -> 
      case Array.get v arr of
        Just (buffer, update) -> 
          { editor 
          | buffers = Array.set i buffer editor.buffers 
          , frame = Maybe.map (Frame.updateFocus update) editor.frame
          }
        Nothing -> 
          Debug.crash "Should Not Happen"
    _ -> editor

doMsg msg model = 
  let 
    fail msg =
      { model | mode = Failed msg model.mode }
  in case msg of
    DoAll actions -> 
      List.foldl doMsg model actions
    
    NoOp -> model

    State -> Debug.log "state" model

    ChooseMsg msg -> 
      case model.mode of 
        Choose i k array -> 
          case msg of 
            Next -> { model | mode = Choose i ((k + 1) % Array.length array) array }
            Prev -> { model | mode = Choose i ((k - 1) % Array.length array) array }
            Done -> 
              let model' = realizeChoice model 
              in { model' | mode = Normal }
        
        _ -> fail "Can not perform this action out of choose mode"

    Fail msg -> 
      fail msg
    
    NewBuffer language -> 
      let
        buffer = Buffer.new language 
        buffers = Array.push buffer model.buffers 
        frame = Frame.new (Array.length buffers - 1, buffer)
      in 
        { model | 
            buffers = buffers, 
            frame = Just frame
        }

    DoBuffer id msg -> 
      case Array.get id model.buffers of
        Just buffer -> 
          case Buffer.update msg buffer of
            Buffer.Options options -> 
              { model | mode = Choose id 0 (Array.fromList options) }
        Nothing ->
          fail <| "Could not find buffer '" ++ toString id ++ "'"

    DoFrame action ->
      case model.frame of
        Just frame -> 
          case Frame.update action frame of 
            Frame.Update frame' -> 
              { model | frame = Just frame' }
            Frame.UpdateWithBuffer id fn -> 
              case Array.get id model.buffers of 
                Just buffer -> 
                  { model | frame = Just (fn buffer) } 
                Nothing -> fail "No buffer"
            Frame.UpdateBuffer i msg -> 
              doMsg (DoBuffer i msg) model
            Frame.Fail str -> 
              fail str
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
        "child" -> 
          f Movement.Child
        "parrent" -> 
          f Movement.Parrent
        "next" -> 
          f Movement.Next
        "prev" -> 
          f Movement.Prev
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

            "state" -> State
            
            _ -> 
              Fail <| "Unknown command '" ++ cmd ++ "'"

        [] -> NoOp

  in Debug.log "Msg" <| parseArgs args

type Mode
  = Normal 
  | Failed String Mode
  | Command String (String -> Msg)
  | Choose Int Int (Array (Buffer, Focus -> Focus))

view : Editor -> Html Msg 
view editor = 
  let 
    size = { height = 400, width = 800 }
    cmdlineHtml = cmdline { height = 20, width = 800} editor.mode
    cmdlineSize = Maybe.withDefault { height = 0, width = 800 }
      <| Maybe.map (always { height = 20, width = size.width }) cmdlineHtml
  in div [ class "ash" , attr size ] <| 
    [ div 
      [ class "editor"
      , attr { size | height = size.height - cmdlineSize.height } 
      ] 
      ( let editor' = realizeChoice editor
        in 
          [ case editor'.frame of
              Just frame -> 
                case Array.get (Frame.getBufferId frame) editor'.buffers of 
                  Just buffer -> Frame.view frame buffer
                  Nothing -> 
                    div [ class "frame" ] 
                      [ div 
                        [ class "no-buffer-frame" ] 
                        [ text "No buffer in frame." ] 
                      ]
              Nothing -> 
                div [ class "frame" ] 
                  [ div 
                    [ class "no-frame" ] 
                    [ text "Welcome to Ash. To continue use :new <language>." ] 
                  ]
          ] 
      )
    ] ++ maybeToList cmdlineHtml

cmdline size mode = 
  case mode of
    Normal -> Nothing
    
    Failed msg mode -> 
      Just <|
        div [ class "cmdline cmdline-failed", attr size ]
          [ p [] [text ("Failed: " ++ msg)] ]
    
    Command pre f -> 
      Just <| 
        div [ class "cmdline", attr size] 
          [ p [] [text pre]
          , input 
            [ onEnter f
            , autofocus True
            , attribute "data-autofocus" ""
            ] [] 
          ]

    Choose i k arr -> 
      Just <| 
        div [ class "cmdline", attr size] 
          [ p [] [text ("Choose ["++ toString (k + 1) ++ "/" ++ toString (Array.length arr) ++ "]" )] 
          ]
    


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

keyHandler : Editor -> Keyboard.KeyCode -> Msg
keyHandler editor key =
  -- let key' = Debug.log "key" key in 
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

    Choose i k array -> 
      case key of
        14 -> ChooseMsg Next 
        16 -> ChooseMsg Prev 
        13 -> ChooseMsg Done
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
    Choose _ _ _ -> NoOp

subscriptions editor = 
  Sub.batch 
    [ Keyboard.presses (keyHandler editor)
    , Keyboard.ups (specialKeyHandler editor)
    ]

editor : Settings -> Program Never
editor settings = 
  program 
    { init = ( debug settings, settings.after )
    , subscriptions = subscriptions
    , update = update 
    , view = view 
    }
