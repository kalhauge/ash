-- Manual translation of 
-- https://github.com/cdglabs/ohm/blob/master/examples/math/index.html
module Arithmetic where

import AST exposing (..)

lang = 
  grammar 
    [ ( "Exp"
      , rule 
        [[ Ref "AddExp" ]]
      )
    , ( "AddExp"
      , rule 
        [ [ Ref "AddExp", Lex "+", Ref "MulExp" ] -- plus
        , [ Ref "AddExp", Lex "-", Ref "MulExp" ] -- minus
        , [ Ref "MulExp" ]
        ]
      )
    , ( "MulExp"
      , rule
        [ [ Ref "MulExp", Lex "*", Ref "ExpExp" ] -- times
        , [ Ref "MulExp", Lex "/", Ref "ExpExp" ] -- divide
        , [ Ref "ExpExp" ]
        ]
      )
    , ( "ExpExp"
      , rule 
        [ [ Ref "PriExp", Lex "^", Ref "ExpExp" ]  -- power
        , [ Ref "PriExp" ] 
        ]
      )
    , ( "PriExp"
      , rule
        [ [ Lex "(", Ref "Exp", Lex ")" ]  -- paren
        , [ Lex "-", Ref "PriExp" ]        -- pos
        , [ Ref "ident" ]
        , [ Ref "number" ]
        ]
      )
    , ( "ident"
      , rule 
        [ [ Ref "alpha", Ref "ident" ]
        , [ Ref "alpha" ]
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
