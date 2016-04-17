import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (style, attribute, autofocus)
import Effects exposing (Never)
import Signal
import StartApp exposing (start)
import Keyboard exposing (presses)

import Array

import Char
import Maybe exposing (Maybe, andThen)
import String

import AST exposing (..)
import Command exposing (..)
import Grammar exposing (..)
import Parser exposing (..)
import Arithmetic 

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
  }

model : Model
model = 
  { tree = empty
  , focus = 1
  , lang = Arithmetic.lang
  , mode = Normal
  , lastKey = 0
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

type Action 
  = KeyPress Char.KeyCode
  | SetChange String

inputs = 
    [ Signal.map KeyPress Keyboard.presses 
    ]

-- Tries to verify a model returns a list of sugested fixes, including the
-- Original model if it is correct.
verify : Grammar -> (Int, SyntaxTree) -> List (Int, SyntaxTree) 
verify lang ((id, tree) as item) = [ item ]

update : Action -> Model -> (Model, Effects.Effects Action)
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



  in (model', Effects.none)

dpprint : Model -> Html
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

pprint : Model -> Html
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

view address model = 
  div [ ]
    [ table 
      [ style 
        [ ("width", "100%") 
        , ("table-layout", "fixed")
        , ("margin", "30pt")
        ] 
      ]
      [ tr [] <| List.map (\f -> 
          td [ style 
               [ ("vertical-align", "bottom")
               , ("text-align", "center")
               ]
            ] [ f model ]
        ) 
        [ dpprint 
        , pprint 
        ]
      ]
    , editorBar address model
    ]

editorBar address model =
  table 
    [ style 
      [ ("background", "lightblue")
      , ("height", "30px")
      , ("position", "absolute")
      , ("bottom", "0px")
      , ("width", "100%")
      , ("vertical-align", "middle")
      , ("font-size", "10pt")
      , ("font-family", "monospace")
      , ("padding", "0 6px")
      ] 
    ] 
    [ tr [] 
      [ td [ style [] ] 
        ( case model.mode of
          Normal -> [ text "normal" ] 
          Change str -> 
            [ text "change:" 
            , input 
              [ on "input" targetValue
                (\str -> 
                    Signal.message address (SetChange str)
                )
              , autofocus True
              , attribute "data-autofocus" ""
              , style 
                [ ("background", "none")
                , ("border", "none")
                , ("font-family", "monospace")
                , ("font-size", "10pt")
                , ("outline", "none")
                ] 
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
        [ style [ ("width", "40px"), ("text-align", "right") ] ] 
        [ text (toString model.focus) ] 
      , td 
        [ style [ ("width", "40px"), ("text-align", "right") ] ] 
        [ text (toString model.lastKey) ] 
      ]
    ]

main = 
  start 
    { init = (model, Effects.none)
    , view = view
    , update = update 
    , inputs = inputs
    } |> .html
