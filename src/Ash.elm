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
import Arithmetic 

type alias Model = 
  { tree : SyntaxTree
  , focus : Int
  , lang : Grammar
  }

model : Model
model = 
  let 
    tree =
      syntax "AddExp" 0
        [ syntax "AddExp" 0
          [ syntax "AddExp" 0
            [ number [1, 2, 3, 4]
            , number [4, 2]
            ] 
          , number [2, 3]
          ]
          , number [3, 4]
        ]
  in { tree = tree
     , focus = tree.size 
     , lang = Arithmetic.lang
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

type Action 
  = NoOp
  | Focus Movement 
  | DeleteFocus

inputs = 
    [ Signal.map keyHandle Keyboard.presses 
    ]

keyHandle keyCode = 
  case (Char.fromCode keyCode) of
    'j' -> Focus SmartIn
    'J' -> Focus In
    'k' -> Focus SmartOut
    'K' -> Focus Out
    'l' -> Focus SmartNext
    'L' -> Focus Next
    'h' -> Focus SmartPrev
    'H' -> Focus Prev
    'd' -> DeleteFocus
    _ -> NoOp

update : Action -> Model -> (Model, Effects.Effects Action)
update action model = 
  let 
    updateWith fn ({focus, tree} as model) =
        let (focus', tree') = fn model.focus model.tree 
        in { model | tree = tree', focus = focus'}

    model' = case action of
      Focus movement -> 
        { model | focus = 
            ( case movement of 
                SmartOut  -> smartOut
                SmartIn   -> smartIn
                SmartNext -> smartNext
                SmartPrev -> smartPrev
                Out       -> parrent
                In        -> child
                Next      -> next
                Prev      -> prev
            ) model.tree model.focus
        }

      DeleteFocus -> updateWith delete model

      _ -> 
        model
          
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
pprint {tree, lang, focus} =
  let
    collector id tree =
      div 
        [ style <| 
          [ ("display", "inline") ] 
            ++ if id == focus then
              [ ("background", "lightgray") ]
            else []
        ]
        ( Maybe.withDefault [ text "?" ] 
          <| translate lang tree (\str -> 
            div  
              [ style <| 
                [ ("display", "inline") ] 
                  ++ if id == focus then
                    [ ("color", "red") ]
                  else []
              ]
              [ text str ]
          ))
  in
     collect collector tree

view address model = 
  table 
    [ style
        [ ("width", "600px")
        , ("margin", "100px auto")
        ]
    ]
    [ tr [] 
      <| List.map 
        (\f -> 
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


main = 
  start 
    { init = (model, Effects.none)
    , view = view
    , update = update 
    , inputs = inputs
    } |> .html
