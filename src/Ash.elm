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
import Arithmetic 

number numbers =
  case numbers of
    [n] -> 
      syntax "digit" n False [] 
    n :: ns -> 
      syntax "number" 1 False 
        [ syntax "digit" n False [] 
        , number ns 
        ]
    [] -> Debug.crash "Bad use!"

type alias Model = SyntaxTree

model : Model
model = 
  let lang = Arithmetic.lang 
  in syntax "AddExp" 0 True 
    [ number [1, 2, 3]
    , number [4, 2]
    ] 

type Action 
  = NoOp
  | FocusOut 
  | FocusIn 
  | Right
  | Left

inputs = 
    [ Signal.map keyHandle Keyboard.presses 
    ]

keyHandle keyCode = 
  case (Char.fromCode keyCode) of
    'j' -> FocusIn
    'k' -> FocusOut
    --'l' -> Right
    --'h' -> Left
    _ -> NoOp

update action model = 
  let model' = case action of
      NoOp -> 
        model

      FocusOut -> 
        focusOut model

      FocusIn -> 
        focusIn model

      _ -> model 
  in (model', Effects.none)

dpprint : Grammar -> SyntaxTree -> Html
dpprint grammar (SyntaxTree tree as st) = 
  let background = 
        if tree.focus then 
           [ ("background", "lightgray") ]
        else []
  in div 
    [ style <|
      [ ("display", "inline-block")
      , ("margin", "2px 2px 0px 2px")
      , ("text-align", "center")
      ] ++ if tree.focus then 
        [ ("background", "lightgray") ]
      else []
    ]
    <| [ div 
      [ style <|
        [ ("font-size", "6pt") ] ++ 
        if tree.focus then 
          [ ("background", "black") 
          , ("color", "lightblue") 
          ]
        else 
          [("background", "lightblue")]
      ] 
      [ text tree.name ] 
    ] ++ (
      Maybe.withDefault [ text "?" ] 
        <| translate grammar st (dpprint grammar) text
    )

pprint : Grammar -> SyntaxTree -> Html
pprint grammar (SyntaxTree tree as st) =
  let background = 
        if tree.focus then 
           [ ("background", "lightgray") ]
        else []
      terms = Maybe.withDefault [ text "?" ] 
        <| translate grammar st (pprint grammar) text
  in div
    [ style <| 
        [ ("display", "inline")
        ] ++ background
    ]
    terms

view address model = 
  table 
    [ style
        [ ("width", "400px")
        , ("margin", "100px auto")
        ]
    ]
    [ tr [] 
      <| List.map 
        (\f -> 
          td [] [ f Arithmetic.lang model ]
        ) 
        [ dpprint 
        , pprint 
        ]
    ]


main = 
  start 
    { init = (model, Effects.none)
    , view = view
    , update = update 
    , inputs = inputs
    } |> .html
