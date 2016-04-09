import Html exposing (..)
import Dict as D
import String
import Array as A
import Maybe exposing (Maybe, andThen)
import StartApp.Simple exposing (start)

type alias Grammar = D.Dict String Rule
type alias Rule    = A.Array Alt
type alias Alt     = List Term

type Term = Ref String 
          | Lex String

type SyntaxTree = 
      Node String (List SyntaxTree)
    | Leaf String

type alias Model = { tree : SyntaxTree }

type Action = NoOp

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

many numbers =
  let x = case numbers of
      [n] -> 
        [ Node "digit" [ Leaf n ] ]
      n :: ns -> 
        [ Node "digit" [ Leaf n ], many ns ]
      [] -> Debug.crash "Bad use!"
  in Node "number" x

model = { language = exprLang
        , tree = Node "AddExpr"
            [ many [ "4", "2" ] 
            , Leaf "+"
            , many [ "2", "3", "6" ] 
            ]
        }

lookup : String -> Int -> Grammar -> Maybe Alt
lookup s i g = D.get s g `andThen` A.get i 

update action model = model

startingUpper str =
  let first = String.left 1 str
  in String.toUpper first == first

pprint : SyntaxTree -> String
pprint tree = 
  case tree of
    Node rulename terms ->
      let s = if startingUpper rulename then " " else "" 
       in String.join s (List.map pprint terms)
    Leaf string ->
        string

view address model = text (pprint model.tree)

main = start { model = model
             , view = view
             , update = update 
             }
