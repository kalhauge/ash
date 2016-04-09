import Html exposing (..)
import Html.Attributes exposing (style)
import Effects exposing (Never)
import Signal
import StartApp exposing (start)
import Keyboard exposing (presses)

import Char
import Array as A
import Dict as D
import Maybe exposing (Maybe, andThen)
import String

type alias Grammar = D.Dict String Rule
type alias Rule    = A.Array Alt
type alias Alt     = List Term

type Term = Ref String 
          | Lex String

type SyntaxTree = 
      Node String { selected : Bool } (List SyntaxTree)
    | Leaf String

exprLang = 
    D.fromList 
        [ ("Exp"
          , A.fromList [[ Ref "AddExp" ]]
          )
        , ("AddExp"
          , A.fromList 
            [ [ Ref "number", Lex "+", Ref "number" ]
            , [ Ref "number", Lex "-", Ref "number" ]
            , [ Ref "number" ]
            ]
          )
        , ("number", number)
        , ("digit", digit)
        ]

number = 
  A.fromList
    [ [ Ref "digit" ] 
    , [ Ref "digit", Ref "number" ]
    ]

digit = 
   A.fromList
    [ [ Lex "0" ]
    , [ Lex "1" ]
    , [ Lex "2" ]
    , [ Lex "3" ]
    , [ Lex "4" ]
    , [ Lex "5" ]
    , [ Lex "6" ]
    , [ Lex "7" ]
    , [ Lex "8" ]
    , [ Lex "9" ]
    ]

node : String -> List SyntaxTree -> SyntaxTree
node str ls =
    Node str { selected = False } ls

number' numbers =
  let x = case numbers of
      [n] -> 
        [ node "digit" [ Leaf n ] ]
      n :: ns -> 
        [ node "digit" [ Leaf n ], number' ns ]
      [] -> Debug.crash "Bad use!"
  in node "number" x

lookup : String -> Int -> Grammar -> Maybe Alt
lookup s i g = D.get s g `andThen` A.get i 

type alias Model = SyntaxTree

model = 
  Node "AddExpr" 
    { selected = True } 
    [ number' [ "4", "2" ] 
    , Leaf "+"
    , number' [ "2", "3", "6", "2"] 
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

focusFirst ss = 
  case ss of
    s :: ss' -> 
      case s of
        Leaf _ ->
          focusFirst ss' `andThen` (\ss'' -> Just (s :: ss''))
        Node n meta ss'' ->
          Just ( (Node n { meta | selected = True } ss'') :: ss')
    [] -> 
      Nothing

getFocus ss = 
  case ss of
    s :: ss' -> 
      case s of
        Leaf _ ->
          getFocus ss' `andThen` (\ss'' -> Just (s :: ss''))
        Node n meta myss ->
          if meta.selected then
            let ss'' = Maybe.withDefault ss' (getFocus ss')
            in Just ( (Node n { meta | selected = False } myss) :: ss'')
          else
            getFocus ss' `andThen` (\ss'' -> Just (s :: ss''))
    [] -> 
      Nothing

moveUp : SyntaxTree -> SyntaxTree
moveUp st =
  case st of
    Leaf _ -> st
    Node n meta ss -> 
      case getFocus ss of
        Nothing -> 
          Node n meta (List.map moveUp ss)
        Just ss' ->
          Node n 
            {meta | selected = True} 
            (List.map moveUp ss')

moveDown : SyntaxTree -> SyntaxTree
moveDown st = 
  case st of
    Leaf _ -> st
    Node n meta ss -> 
      let ss' = (List.map moveDown ss)
      in if meta.selected then
        case focusFirst ss' of
          Just ss'' -> 
             Node n { meta | selected = False } ss''
          Nothing ->
            Node n meta ss' 
      else 
        Node n meta ss' 

-- moveRight : SyntaxTree -> SyntaxTree
--   case st of
--     Leaf _ -> st
--     Node n meta ss -> 
--       let ss' = (List.map moveRight ss)
--       in 

update action model = 
  let model' = case action of
      NoOp -> model
      Up -> moveUp model
      Down -> moveDown model
      _ -> model
  in (model', Effects.none)

startingUpper str =
  let first = String.left 1 str
  in String.toUpper first == first

pprint : SyntaxTree -> Html
pprint tree = 
  case tree of
    Node rulename meta terms ->
      let dist = if startingUpper rulename then "2px" else "0px" 
      in div 
        [ style (
           [ ("display", "inline-block")
           , ("margin", "2px 2px")
           , ("text-align", "center")
           ] ++ if meta.selected 
               then [ ("background", "lightgray") ]
               else []
           )
        ]
        ( [ 
            div 
              [ style (
                [ ("background", "lightblue")
                , ("font-size", "6pt")
                ] ++ if meta.selected 
                then [ ("background", "black") 
                     , ("color", "lightblue") 
                     ]
                else []
                )
              ] 
              [ text rulename ] 
            ]
           ++ (List.map pprint terms) 
         )
    Leaf string ->
      div 
        [ style 
            [ ("display", "inline-block") 
            ]
        ] 
        [text string ]

view address model = 
  div 
    [ style 
       [ ("margin", "100px auto")
       , ("width", "400px")
       --, ("text-align", "center")
       ]
    ] 
    [ pprint model ]

main = ( start { init = (model, Effects.none)
             , view = view
             , update = update 
             , inputs = inputs
             } ).html
