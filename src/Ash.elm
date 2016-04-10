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

exprLang = 
  grammar 
    [ ( "Exp"
      , rule 
        [[ Ref "AddExp" ]]
      )
    , ( "AddExp"
      , rule 
        [ [ Ref "AddExp", Lex "+", Ref "number" ]
        , [ Ref "AddExp", Lex "-", Ref "number" ]
        , [ Ref "number" ]
        ]
      )
    , ( "number"
      , rule 
        [ [ Ref "digit" ] 
        , [ Ref "digit", Ref "number" ]
        ]
      )
    , ( "digit"
      , oneOf "0123456789" 
      )
    ]

number numbers =
  case numbers of
    [n] -> 
      syntax ("number", 0) False 
        [ syntax ("digit", n) False [] 
        ]
    n :: ns -> 
      syntax ("number", 1) False 
        [ syntax ("digit", n) False [] 
        , number ns 
        ]
    [] -> Debug.crash "Bad use!"

type alias Model = SyntaxTree

model = 
  syntax ("AddExp", 0) True 
    [ syntax ("AddExp", 2) False 
        [ number [1, 2, 3] ]
    , number [4, 3]
    ] 

type Action 
  = NoOp
  | Up
  | Down 
  | Right
  | Left

inputs = 
    [ Signal.map keyHandle Keyboard.presses ]

keyHandle keyCode = 
  case (Char.fromCode keyCode) of
    'j' -> Down
    'k' -> Up
    'l' -> Right
    'h' -> Left
    _ -> NoOp

update action model = (model, Effects.none)
--   let model' = case action of
--       NoOp -> 
--         model
-- 
--       Up -> 
--         moveUp model
-- 
--       Down -> 
--         moveDown model
-- 
--       Right -> 
--         moveRight model
-- 
--       Left -> 
--         moveLeft model
--   in (model', Effects.none)
-- 
-- startingUpper str =
--   let first = String.left 1 str
--   in String.toUpper first == first
-- 
-- dpprint : SyntaxTree -> Html
-- dpprint tree = 
--   case tree of
--     Node rulename meta terms ->
--       div 
--         [ style (
--            [ ("display", "inline-block")
--            , ("margin", "2px 2px 0px 2px")
--            , ("text-align", "center")
--            ] ++ if meta.selected 
--                then [ ("background", "lightgray") ]
--                else []
--            )
--         ]
--         ( [ div 
--               [ style (
--                 [ ("background", "lightblue")
--                 , ("font-size", "6pt")
--                 ] ++ if meta.selected 
--                 then [ ("background", "black") 
--                      , ("color", "lightblue") 
--                      ]
--                 else []
--                 )
--               ] 
--               [ text rulename ] 
--           ] ++ (List.map dpprint terms) 
--          )
--     Leaf string ->
--       div 
--         [ style 
--             [ ("display", "inline") 
--             ]
--         ] 
--         [text string ]


pprint : Grammar -> SyntaxTree -> Html
pprint grammar (SyntaxTree tree as st) =
  let background = 
        if tree.focus then 
           [ ("background", "lightgray") ]
        else []
      terms = Maybe.withDefault [ text "?" ] 
        <| runs grammar st (pprint grammar) text
  in div
    [ style <| 
        [ ("display", "inline")
        ] ++ background
    ]
    terms

view address model = 
  div 
    [ style 
       [ ("margin", "100px auto")
       , ("width", "500px")
       , ("font-family", "monospace")
       ]
    ] 
    [ pprint exprLang model ]

main = 
  start 
    { init = (model, Effects.none)
    , view = view
    , update = update 
    , inputs = inputs
    } |> .html
