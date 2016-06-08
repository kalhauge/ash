module Ash.Editor exposing (..)

{- 
This module contains the main data structure for controling the editor.
-}

import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (class, autofocus, style, attribute, value)
import Html.Events exposing (onInput, on, onWithOptions, defaultOptions)

import Style exposing (..)

import Json.Decode as Json

import Platform exposing (Program)
import Platform.Cmd exposing (Cmd(..), none, (!))

import Keyboard exposing (presses)

import Lazy.List as LazyList exposing (LazyList) 
import Lazy

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
  let 
    languages = Language.collect settings.languages
    buffer = Buffer.Buffer 
      { data = 
        { kind = ("Expr",2), terms = [SubTree { kind = ("Expr",0), terms = [SubTree { kind = ("LetAssign",0), terms = [SubTree { kind = ("alpha",0), terms = [], size = 1 },SubTree { kind = ("number",0), terms = [SubTree { kind = ("digit",2), terms = [], size = 1 },SubTree { kind = ("digit",3), terms = [], size = 1 }], size = 3 }], size = 5 },SubTree { kind = ("alpha",0), terms = [], size = 1 }], size = 7 },SubTree { kind = ("alpha",0), terms = [], size = 1 },SubTree { kind = ("alpha",0), terms = [], size = 1 }], size = 10 }
      , language = case Dict.get "f" languages of
          Just m -> m
          Nothing -> Debug.crash "Where is math"
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
  | WithInput String String (String -> Editor -> Msg)

type ChooseMsg 
  = Next
  | Prev
  | Done
  | UpdateInput String

realizeChange : Editor -> Editor
realizeChange editor = 
  case editor.mode of  
    Change opts -> 
      case Array.get opts.index opts.options of
        Just a -> activateBufferUpdate opts.bufferId a editor
        Nothing -> 
          editor
    _ -> editor

activateBufferUpdate : Int -> (Buffer, Int -> Int) -> Editor -> Editor
activateBufferUpdate i (buffer, update) editor =
  { editor 
  | buffers = Array.set i buffer editor.buffers 
  , frame = Maybe.map (Frame.updateFocus update) editor.frame
  }

navigateToEmptyAndDo : Int -> Msg -> Editor -> Editor
navigateToEmptyAndDo i msg editor =
  case Array.get i editor.buffers of 
    Just buffer -> 
      case Buffer.findEmpty buffer of
        Just empty -> 
          flip doMsg editor
          <| DoAll 
             [ DoFrame (Frame.SetFocus empty)
             , msg 
             ]
        Nothing ->
          editor
    Nothing -> 
      failMode editor ("Could not find buffer " ++ toString i)

failMode : Editor -> String -> Editor
failMode model msg =
  { model | mode = Failed msg model.mode }

doMsg msg model = 
  let 
    fail = 
      failMode model
    setMode mode = 
      { model | mode = mode }
  in case msg of
    NoOp -> model
    
    DoAll actions -> 
      List.foldl doMsg model actions

    State -> Debug.log "state" model
    
    ChooseMsg msg -> 
      case model.mode of 
        Change opts -> 
          case msg of 
            Next -> 
              if opts.index + 1 == Array.length opts.options then
                case Lazy.force opts.more of
                  LazyList.Cons e more -> 
                    setMode 
                      (Change 
                        { opts 
                        | index = (opts.index + 1) 
                        , more = more
                        , options = Array.push e opts.options
                        }
                      )
                  LazyList.Nil ->
                    setMode 
                      (Change {opts | index = (opts.index + 1) % Array.length opts.options})
              else
                if not <| Array.isEmpty opts.options then
                  setMode (Change {opts | index = (opts.index + 1) % Array.length opts.options})
                else
                  model
            Prev -> 
              if not <| Array.isEmpty opts.options then
                setMode (Change {opts | index = (opts.index - 1) % Array.length opts.options})
              else
                model 
            Done -> 
              let model' = realizeChange model 
              in navigateToEmptyAndDo opts.bufferId
                (DoFrame (Frame.OnBufferWithFocus (Buffer.Change "")))
                { model' | mode = Normal }
            UpdateInput i -> 
              setMode (Change { opts | input = i })
              |> doMsg (DoFrame <| Frame.OnBufferWithFocus <| Buffer.Change i) 
        
        _ -> fail "Can not perform this action out of change mode"

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
              case model.mode of
                Change opts -> 
                  setMode <| Change 
                    { opts 
                    | index = 0
                    , more = LazyList.empty
                    , options = Array.fromList options 
                    }
                _ -> 
                  case options of 
                    [] -> fail "not possible"
                    [a] -> 
                      activateBufferUpdate id a model
                    _ ->
                      setMode <| Change 
                        { index = 0
                        , input = ""
                        , bufferId = id
                        , more = LazyList.empty
                        , options = Array.fromList options
                        }
            Buffer.LazyOptions options -> 
                case model.mode of
                  Change opts -> 
                    setMode <| Change 
                      { opts
                      | index = 0
                      , bufferId = id
                      , more = LazyList.drop 1 options
                      , options = LazyList.toArray <| LazyList.take 1 options
                      }
                  _ -> 
                    setMode <| Change 
                      { index = 0
                      , input = ""
                      , bufferId = id
                      , more = LazyList.drop 1 options
                      , options = LazyList.toArray <| LazyList.take 1 options
                      }


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
      setMode mode

    WithInput pre input f -> 
      setMode <| Command pre input (\str -> DoAll [SetMode Normal, f str model]) 

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
                  DoFrame (Frame.OnBufferWithFocus <| Buffer.Replace expr)
                _ -> badArgs 
            
            "delete" -> 
              DoFrame (Frame.OnBufferWithFocus <| Buffer.Delete)
            
            "change" -> 
              DoFrame (Frame.OnBufferWithFocus <| Buffer.Change "")
            
            "append" -> 
              DoFrame (Frame.OnBufferWithFocus <| Buffer.Append)

            "keep" -> 
              DoFrame (Frame.OnBufferWithFocus <| Buffer.Keep)
            
            "insert" -> 
              DoFrame (Frame.OnBufferWithFocus <| Buffer.Insert)

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
                  WithInput pre "" (\i -> parseMsg (String.join " " rest ++ " " ++ i))
                _ -> badArgs
            
            "command" ->
              parseArgs rest 

            "display" -> 
              case rest of
                [serializer] -> 
                  DoFrame (Frame.SetSerializer serializer)
                _ -> badArgs

            "state" -> State
            
            "dump" -> DoFrame Frame.DumpData
            
            _ -> 
              Fail <| "Unknown command '" ++ cmd ++ "'"

        [] -> NoOp

  in Debug.log "Msg" <| parseArgs args

type Mode
  = Normal 
  | Failed String Mode
  | Command String String (String -> Msg)
  | Change 
    { input : String
    , bufferId : Int
    , index : Int
    , options : Array (Buffer, Focus -> Focus)
    , more : LazyList (Buffer, Focus -> Focus)
    }

changeMode : Int -> String -> LazyList (Buffer, Focus -> Focus) -> Mode
changeMode id input more =
  Change 
    { input = input 
    , bufferId = id 
    , index = 0 
    , options = Array.empty 
    , more = more 
    }

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
      ( let editor' = realizeChange editor
        in 
          [ case editor'.frame of
              Just frame -> 
                case Array.get (Frame.getBufferId frame) editor'.buffers of 
                  Just buffer -> 
                    Html.App.map (always NoOp) <| Frame.view frame buffer
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
    
    Command pre existing f -> 
      Just <| 
        div [ class "cmdline", attr size] 
          [ p [] [text pre]
          , input 
            [ onEnter f
            , onInput (\i -> SetMode <| Command pre i f)
            , value existing
            , autofocus True
            , attribute "data-autofocus" ""
            ] [] 
          ]

    Change opts -> 
      Just <| 
        div [ class "cmdline", attr size] 
          [ p [] [ text "CHANGE:" ]
          , input 
            [ onEnter (always (ChooseMsg Done))
            , onInput (\i -> (ChooseMsg (UpdateInput i)))
            , value opts.input
            , autofocus True
            , attribute "data-autofocus" ""
            ] [] 
          , p [] 
            [ text ("["++ toString (opts.index + 1) 
                ++ "/" ++ toString (Array.length opts.options) ++ 
                if not <| LazyList.isEmpty opts.more then "+]" else "]"
              )
            ] 
          ]

valueOnKey : Keyboard.KeyCode -> (String -> msg) -> Attribute msg
valueOnKey key tagger =
  let
    onlyOnKey = 
      Json.object2 (,) 
        Html.Events.keyCode 
        Html.Events.targetValue
        `Json.andThen` \(a,b) -> 
      if a == key then 
         Json.succeed b 
      else 
         Json.fail "only fire on key"
      
  in onWithOptions "keydown" 
        { stopPropagation = True
        , preventDefault = True } 
      <| Json.map tagger onlyOnKey

onEnter = 
  valueOnKey 13 

onCtrlN =
  valueOnKey 14

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

    Command pre _ f ->
      case key of
        96 -> SetMode Normal
        _ -> NoOp
    
    Change opts -> 
      case key of
        14 -> ChooseMsg Next 
        16 -> ChooseMsg Prev 
        13 -> ChooseMsg Done
        _ -> NoOp


specialKeyHandler : Editor -> Keyboard.KeyCode -> Msg
specialKeyHandler editor key =
  -- let key' = Debug.log "specialkey" key in 
  case editor.mode of
    Normal ->
      case key of
        37 -> DoFrame (Frame.SmartFocus Movement.Prev) -- left
        38 -> DoFrame (Frame.SmartFocus Movement.Parrent) -- up 
        39 -> DoFrame (Frame.SmartFocus Movement.Next) -- right
        40 -> DoFrame (Frame.SmartFocus Movement.Child) -- down
        _ -> NoOp
    Failed msg mode -> NoOp
    Command pre _ f -> 
      case key of
        27 -> SetMode Normal
        _ -> NoOp
    Change _ -> 
      case key of
        14 -> ChooseMsg Next 
        16 -> ChooseMsg Prev 
        38 -> ChooseMsg Prev 
        40 -> ChooseMsg Next 
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
    { init = ( debug settings, settings.after )
    , subscriptions = subscriptions
    , update = update 
    , view = view 
    
  }
