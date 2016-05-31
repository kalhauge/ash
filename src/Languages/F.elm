module Languages.F exposing (language)

import Html exposing (..)
import Html.Attributes exposing (class)

import Style exposing (classes)

import Array
import String

import Ash.SyntaxTree as SyntaxTree

import Ash.Grammar as Grammar exposing (..)
import Ash.Language as Language

language = Language.new 
  { name = "f"
  , grammar = grammar
  , headExpr = "Expr"
  , serializers = []
  , defaultSerializer = "simple"
  }

grammar = 
  Grammar.grammar
    [ ( "Expr"
      , rule 
        [ [ Lex "let", Ref "ident", Lex "=", Ref "Expr", Lex "in", Ref "Expr" ] 
        , [ Lex "fun", Ref "ident", Ref "Args", Lex "->", Ref "Expr" ] 
        , [ Lex "if", Ref "Expr", Lex "then", Ref "Expr", Lex "else", Ref "Expr" ]
        , [ Ref "OrExpr" ]
        ]
      )
    , ( "Args"
      , rule 
        [ [ Ref "ident" ]
        , [ Ref "Args", Ref "ident" ]
        ]
      )
    , ( "OrExpr"
      , rule
        [ [ Ref "OrExpr", Lex "||",  Ref "AndExpr" ]  -- or
        , [ Ref "AndExpr" ]
        ]
      )
    , ( "AndExpr"
      , rule 
        [ [ Ref "AndExpr", Lex "&&", Ref "EqExpr"]  -- and
        , [ Ref "EqExpr" ]
        ]
      )
    , ( "EqExpr"
      , rule
        [ [ Ref "RelExpr" ]
        , [ Ref "RelExpr", Lex "=", Ref "RelExpr" ]  -- eq
        , [ Ref "RelExpr", Lex "!=", Ref "RelExpr" ]  -- neq
        ]
      )
    , ( "RelExpr"
      , rule 
        [ [ Ref "AddExpr" ]
        , [ Ref "AddExpr", Lex "<", Ref "AddExpr" ]  -- lt
        , [ Ref "AddExpr", Lex ">", Ref "AddExpr" ]  -- gt
        ]
      )
    , ( "AddExpr"
      , rule
        [ [ Ref "AddExpr", Lex "+", Ref "MulExpr" ]  -- plus
        , [ Ref "AddExpr", Lex "-", Ref "MulExpr" ]  -- minus
        , [ Ref "MulExpr" ]
        ]
      )
    , ( "MulExpr"
      , rule
        [ [ Ref "MulExpr", Lex "*", Ref "CallExpr" ]  -- times
        , [ Ref "MulExpr", Lex "/", Ref "CallExpr" ]  -- divide
        , [ Ref "MulExpr", Lex "%", Ref "CallExpr" ]  -- modulus
        , [ Ref "CallExpr" ]
        ]
      )
    , ( "CallExpr"
      , rule 
        [ [ Ref "UnExpr" ]
        , [ Ref "UnExpr", Ref "CallArgs" ]   -- args
        ]
      )
    , ( "CallArgs"
      , rule
        [ [ Ref "PriExpr" ]
        , [ Ref "PriExpr", Ref "CallArgs" ]
        ]
      )
    , ( "UnExpr"
      , rule
        [ [ Lex "+", Ref "PriExpr" ]    -- pos
        , [ Lex "-", Ref "PriExpr" ]    -- neg
        , [ Lex "delay", Ref "PriExpr" ]  -- delay
        , [ Lex "force", Ref "PriExpr" ]  -- force
        , [ Ref "PriExpr" ]
        ]
      )
    , ( "PriExpr"
      , rule
        [ [ Lex "(", Ref "Expr", Lex ")" ]                                  -- paren
        -- , [ Lex "[", Expr "|" ident "<-" Expr ("," Expr)? "]"  -- listComp
        -- , [ "[" ListOf<Expr, ";"> "]"                     -- list
        -- , [ ctor "(" ListOf<Expr, ","> ")"                -- datum
        -- , [ ctor ~"("                                     -- emptyDatum
        , [ Ref "ident" ]                                    -- ident
        , [ Ref "number"]                                    -- number
        , [ Lex "True"  ]                                    -- true
        , [ Lex "False" ]                                    -- false
        ]
      )
    , ( "ident"
      , rule 
        [ [ Ref "alpha", Ref "identRest" ]
        , [ Ref "alpha" ]
        ]
      )
    , ( "identRest"
      , rule
        [ [ Ref "alnum", Ref "identRest" ]
        , [ Ref "alnum" ]
        ]
      )
    , ( "alnum"
      , rule 
        [ [ Ref "alpha" ]
        , [ Ref "number" ]
        ]
      )
    , ( "alpha"
      , oneOf "abcdefghijklmnopqrstuvxyz" 
      )
    , ( "number"
      , rule 
        [ [ Ref "digit", Ref "number" ]
        , [ Ref "digit" ] 
        ]
      )
    , ( "digit"
      , oneOf "0123456789" 
      )
    ]

-- printer {date, grammar, focus} = 
--   let 
--     collector id tree =
--       let 
--         grp name = 
--           node name (if id == focus then [ class "ash-focus"] else [])
--       
--         operator opr =
--           grp "operator" [ text opr ]
-- 
--         alphabeth = 
--           Array.fromList (String.toList "abcdefghijklmnopqrstuvxyz" |> List.map String.fromChar)
--       
--       in 
--         case tree.kind of
--           _ -> div [] []
-- 
--   in div 
--       [ class "ash-math" ] 
--       [ SyntaxTree.collect collector data ]

