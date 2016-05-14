port module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (style, attribute, autofocus, id, class)
import Platform.Cmd exposing (..)
import Html.App exposing (program)
import Keyboard exposing (presses)

import Array

import Char
import Maybe exposing (Maybe, andThen)
import String

import SyntaxTree exposing (..)
import Command exposing (..)
import Grammar exposing (..)
import Parser exposing (..)
import Languages.Arithmetic 

type Mode 
  = Normal
  | Change String
  | Choose (Int, (Array.Array (Int, SyntaxTree)))

type alias Model = 
  { tree : SyntaxTree
  , focus : Int
  , lang : Grammar
  , mode : Mode
  , lastKey : Char.KeyCode
  , buffers : List Buffer 
  }

type Buffer = Buffer (Model -> Html Msg)

model : Model
model = 
  { tree = empty
  , focus = 1
  , lang = Languages.Arithmetic.lang
  , mode = Normal
  , lastKey = 0
  , buffers = [ Buffer dpprint, Buffer pprint ]
  }

type Movement 
  = Out 
  | SmartOut
  | In
  | SmartIn
  | Next
  | SmartNext
  | Prev
  | SmartPrev

type Msg 
  = KeyPress Char.KeyCode
  | SetChange String

-- Tries to verify a model returns a list of sugested fixes, including the
-- Original model if it is correct.
verify : Grammar -> (Int, SyntaxTree) -> List (Int, SyntaxTree) 
verify lang ((id, tree) as item) = [ item ]

update : Msg -> Model -> (Model, Cmd Msg)
update action model = 
  let 
    updateWith : (Int -> SyntaxTree -> (Int, SyntaxTree)) -> Model -> Model
    updateWith fn ({focus, tree, lang} as model) =
        let options = verify lang (fn focus tree)
        in case options of
            [ (focus, tree) ] -> 
              { model | tree = tree, focus = focus}
            [] -> model
            a -> { model | mode = Choose (0, Array.fromList a) }
  
    focus fn ({focus, tree} as model) =
      { model | focus = fn tree focus }

    model' = case model.mode of
      Normal -> 
        case action of 
          KeyPress key -> 
            let model'' = case (Char.fromCode key) of
              'c' -> 
                updateWith 
                  (Command.update (\_ -> empty)) 
                  { model | mode = Change "" }
              'd' -> 
                updateWith delete model
              'l' -> focus smartNext model
              'h' -> focus smartPrev model
              'j' -> focus smartChild model
              'k' -> focus smartParrent model
              'L' -> focus next model
              'H' -> focus prev model
              'J' -> focus child model
              'K' -> focus parrent model
              _ -> model
            in { model'' | lastKey = key }
          _ -> model
      Change str -> 
        case action of 
          SetChange str -> { model | mode = Change str }
          KeyPress 13 ->
            updateWith (Command.update (\_ -> 
                parse model.lang "Exp" str
                  |> Maybe.map (trim model.lang)
                  |> Maybe.withDefault empty 
                )) 
              { model | mode = Normal, lastKey = 13 }
          KeyPress key -> { model | lastKey = key }
      Choose (i, list) ->
          case action of 
            KeyPress 13 ->
              case  Array.get i list of
                Just (focus, tree) ->
                  { model | mode = Normal, focus = focus, tree = tree }
                _ -> model
            KeyPress key ->
              let model'' = case (Char.fromCode key) of
                'l' -> { model | mode = Choose((i + 1) % Array.length list, list) }
                'h' -> { model | mode = Choose((i - 1) % Array.length list, list) }
                _ ->  model
              in { model'' | lastKey = key }
            _ -> model



  in (model', none)

dpprint : Model -> Html msg
dpprint {tree, lang, focus} = 
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
          [ text (tree.name ++ " : " ++ toString id)] 
        ] ++ (
          Maybe.withDefault [ text "?" ] 
            <| translate lang tree (\str -> 
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
  in collect collector tree

pprint : Model -> Html msg
pprint {tree, lang, focus, mode} =
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
            <| translate lang tree (\str -> 
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
     collect collector tree

view model = 
  div [ class "editor" ]
    [ table [ class "mainView" ]
      [ tr [] 
        <| List.map (\(Buffer f) -> 
          td [ ] [ f model ]
        ) 
        model.buffers
      ]
    , editorBar model
    ]

editorBar model =
  table 
    [ id "editorBar" ] 
    [ tr [] 
      [ td [] 
        ( case model.mode of
          Normal -> [ text "normal" ] 
          Change str -> 
            [ text "change:" 
            , input 
              [ onInput SetChange 
              , autofocus True
              , attribute "data-autofocus" ""
              , id "changeInputField"
              ] []
            ]
          Choose (i, list) -> 
            [ text 
              ( "choose " ++ "[" 
                ++ toString (i + 1) ++ "/" 
                ++ toString (Array.length list) 
                ++ "]"
              )
            ]
        ) 
      , td 
        [ class "rightData"  ] 
        [ text (toString model.focus) ] 
      , td 
        [ class "rightData"  ] 
        [ text (toString model.lastKey) ] 
      ]
    ]

subscriptions = presses KeyPress 

port onload : String -> Cmd msg

main = 
  program 
    { init = (model, onload "hi")
    , view = view 
    , update = update
    , subscriptions = (\m -> subscriptions)
    } 
