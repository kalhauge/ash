import Html exposing (..)
import Html.Attributes exposing (style)
import Effects exposing (Never)
import Signal
import StartApp exposing (start)
import Keyboard exposing (presses)

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

number numbers =
  case numbers of
    [n] -> 
      syntax "digit" n [] 
    n :: ns -> 
      syntax "number" 0
        [ syntax "digit" n [] 
        , number ns 
        ]
    [] -> Debug.crash "Bad use!"

type Movement 
  = Out 
  | SmartOut
  | In
  | SmartIn
  | Next
  | SmartNext
  | Prev
  | SmartPrev

type alias Action = Char.KeyCode

inputs = 
    [ Keyboard.presses 
    ]

addChar str key = 
  String.append str (String.fromChar (Char.fromCode key))

update : Action -> Model -> (Model, Effects.Effects Action)
update action model = 
  let 
    updateWith : (Int -> SyntaxTree -> (Int, SyntaxTree)) -> Model -> Model
    updateWith fn ({focus, tree} as model) =
        let (focus', tree') = fn focus tree 
        in { model | tree = tree', focus = focus'}
  
    focus fn ({focus, tree} as model) =
      { model | focus = fn tree focus }

    model' = case model.mode of
      Normal -> 
        case (Char.fromCode action) of
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
      Change str -> 
        case action of
          13 -> 
            updateWith (Command.update (\_ -> 
                parse model.lang "Exp" str
                  |> Maybe.map (trim model.lang)
                  |> Maybe.withDefault empty 
                )) 
              { model | mode = Normal }
          _ ->  
            { model | mode = Change (addChar str action) }

  in ({ model' | lastKey = action }, Effects.none)

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
                Normal -> text "-"
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
  div 
    [ style
      [ ("width", "80%")
      , ("margin", "100px auto")
      ]
    ]
    [ div [ style [] ] 
      [ div 
        [ style 
          [ ("width", "40px")
          , ("display", "inline-block") 
          ] 
        ] 
        [ text (toString model.lastKey) ] 
      , text (toString model.mode)
      ]
    , table 
      [ style 
        [ ("width", "100%") 
        , ("table-layout", "fixed")
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
    ]


main = 
  start 
    { init = (model, Effects.none)
    , view = view
    , update = update 
    , inputs = inputs
    } |> .html
