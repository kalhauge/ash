module Languages.F exposing (language)

import Html exposing (..)
import Html.Attributes exposing (class)

import Style exposing (classes)

import Array
import String

import Ash.SyntaxTree as SyntaxTree exposing (SyntaxTree)

import Ash.Grammar as Grammar exposing (..)
import Ash.Language as Language

import Ash.Serializer as Serializer exposing (Encoding(..), encode)

language = Language.new 
  { name = "f"
  , grammar = grammar
  , headExpr = "Expr"
  , serializers = [("pretty", encode encoder)]
  , defaultSerializer = "pretty"
  }

grammar = 
  Grammar.grammar "?"
    [ ( "Expr"
      , rule 
        [ [ Lex "let", Ref "LetAssigns", Lex "in", Ref "Expr" ] 
        , [ Lex "fun", Ref "Args", Lex "->", Ref "Expr" ] 
        , [ Lex "if", Ref "Expr", Lex "then", Ref "Expr", Lex "else", Ref "Expr" ]
        , [ Ref "OrExpr" ]
        ]
      )
    , ( "LetAssigns"
      , rule 
        [ [ Ref "LetAssign" ]
        , [ Ref "LetAssign", Ref "LetAssigns" ] 
        ]
      )
    , ( "LetAssign"
      , rule [ [ Ref "ident", Lex "=", Ref "Expr" ] ]
      )
    , ( "Args"
      , rule 
        [ [ Ref "Args", Ref "ident" ]
        , [ Ref "ident" ]
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
        [ [ Ref "CallExpr", Ref "PriExpr" ]   -- args
        , [ Ref "UnExpr" ]
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
        , [ Lex "True"  ]                                    -- true
        , [ Lex "False" ]                                    -- false
        , [ Ref "ident" ]                                    -- ident
        , [ Ref "number"]                                    -- number
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

encoder : SyntaxTree -> Encoding
encoder = 
  let
    alphabeth = 
      Array.fromList (
        String.toList "abcdefghijklmnopqrstuvxyz" 
          |> List.map String.fromChar
        )
    
    take1 tree f =
      case tree.terms of
        [ a ] -> f a 
        _ -> Error

    take1l tree f =
      case tree.terms of
        [ a ] -> f a 
        _ -> [ Error ]
    
    take2 tree f =
      case tree.terms of
        [ a, b ] -> f a b
        _ -> Error 

    take2l tree f =
      case tree.terms of
        [ a, b ] -> f a b
        _ -> [ Error ]
    
    take3l tree f =
      case tree.terms of
        [ a, b, c] -> f a b c
        _ -> [ Error ]

    collector id tree =
      let 
          opr n = tree `take2` \a b -> 
            Syntax id [ a, Token n, b ]

      in
      case tree.kind of 
        ("Expr", n) -> 
          Syntax id <| case n of
            0 -> tree `take2l` \asg expr -> 
              [ Group 0 [ Token "let" ] 
              , Group 1 [ asg ]
              , Group 0 [ Token "in" ]
              , Group 1 [ expr ]
              ]
            1 -> tree `take2l` \args expr ->
              [ Group 0 [ Token "fun", args, Token "->" ] 
              , Group 1 [ expr ]
              ]
            2 -> tree `take3l` \if_ then_ else_ ->
              [ Group 0 [ Token "if",  if_ ]
              , Group 0 [ Token "then" ] 
              , Group 1 [ then_] 
              , Group 0 [ Token "else" ]
              , Group 1 [ else_] 
              ]
            _ -> [ Error ]
        
        ("LetAssigns", 1) -> tree `take2` \fst rst ->
          Syntax id 
            [ Group 0 [fst], rst ] 
        
        ("LetAssign", 0) -> tree `take2` \idnt expr ->
          Syntax id 
            [ idnt, Token "=" , expr ] 


        ("Args", 0) -> tree `take2` \args idnt -> 
            Syntax id [ args, idnt ]
        

        ("OrExpr", 0) -> opr "||"
        
        ("AndExpr", 0) -> opr "&&"

        ("EqExpr", 1) -> opr "="
        ("EqExpr", 2) -> opr "!="

        ("RelExpr", 1) -> opr "<"
        ("RelExpr", 2) -> opr ">"

        ("AddExpr", 0) -> opr "+"
        ("AddExpr", 1) -> opr "-"

        ("MulExpr", 0) -> opr "*"
        ("MulExpr", 1) -> opr "/"
        ("MulExpr", 2) -> opr "%"

        ("CallExpr", 0) -> tree `take2` \left right -> 
          Syntax id [ left, right ]

        ("UnExpr", n) -> 
          Lexical id <| case n of 
            0 -> tree `take1l` \a -> [ Token "+", a ] 
            1 -> tree `take1l` \a -> [ Token "-", a ] 
            2 -> [ Token "delay" ] 
            3 -> [ Token "force" ] 
            _ -> [ Error ]
        ("PriExpr", n) -> 
          Lexical id <| case n of 
            0 -> tree `take1l` \a -> [ Token "(", a, Token ")"] 
            1 -> [ Token "True" ] 
            2 -> [ Token "False" ]
            _ -> [ Error ]

        ("number", 0) -> tree `take2` \d n -> 
          Lexical id [ d, n ]
        ("digit", n) -> 
          Lexical id [ Token (toString n) ]
        
        ("ident", 0) -> tree `take2` \a r -> 
          Lexical id [ a, r ]
        ("identRest", 0) -> tree `take2` \a r -> 
          Lexical id [ a, r ]
        
        ("alpha", n) ->
          case Array.get n alphabeth of
            Just a -> Lexical id [ Token a ]
            Nothing -> Error

        ("empty", n) -> Syntax id [ Empty ]
        _ -> Error 

  in SyntaxTree.collect collector



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

