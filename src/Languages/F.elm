module Languages.F exposing (language)

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
        [ [ Ref "RelExpr", Lex "=", Ref "RelExpr" ]  -- eq
        , [ Ref "RelExpr", Lex "!=", Ref "RelExpr" ]  -- neq
        , [ Ref "RelExpr" ]
        ]
      )
    , ( "RelExpr"
      , rule 
        [ [ Ref "AddExpr", Lex "<", Ref "AddExpr" ]  -- lt
        , [ Ref "AddExpr", Lex ">", Ref "AddExpr" ]  -- gt
        , [ Ref "AddExpr" ]
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
        [ [ Ref "UnExpr", Ref "PriExpr", Ref "CallArgs" ]   -- args
        , [ Ref "UnExpr" ]
        ]
      )
    , ( "CallArgs"
      , rule
        [ [ Ref "PriExpr", Ref "CallArgs" ]
        , [ Ref "PriExpr" ]
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
        , [ Lex "true"  ]                                    -- true
        , [ Lex "false" ]                                    -- false
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
