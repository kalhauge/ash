-- Manual translation of 
-- https://github.com/cdglabs/ohm/blob/master/examples/math/index.html
module Languages.Math exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)

import Style exposing (classes)

import Array
import String

import Ash.SyntaxTree as SyntaxTree

import Ash.Grammar as Grammar exposing (..)
import Ash.Language as Language exposing (..)

language : Language
language = Language.new
  { name = "math"
  , grammar = grammar
  , headExpr = "Exp"
  , serializers = [("pretty", printer)]
  , defaultSerializer = "pretty"
  }

grammar = 
  Grammar.grammar 
    [ ( "Exp"
      , rule 
        [ [ Ref "AddExp" ] 
        ]
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

printer {data, grammar, focus} =
  let 
    mallformed = 
      div [] [ text "mallformed" ]
    
    take1 tree f =
      case tree.terms of
        [ a ] -> f a 
        _ -> mallformed

    take2 tree f =
      case tree.terms of
        [ a, b ] -> f a b
        _ -> mallformed
    
    collector id tree =
      let 
        grp name = 
          node name (if id == focus then [ class "ash-focus"] else [])
      
        operator opr =
          grp "operator" [ text opr ]

        alphabeth = 
          Array.fromList (String.toList "abcdefghijklmnopqrstuvxyz" |> List.map String.fromChar)
      
      in 
        case tree.kind of
          ("AddExp", 0) -> tree `take2` \a b ->  
            grp "add"
              [ a, operator "+", b ] 

          ("AddExp", 1) -> tree `take2` \a b -> 
            grp "minus"
              [ a, operator "-", b ] 

          ("MulExp", 0) -> tree `take2` \a b -> 
            grp "multiply"
              [ a, operator "⋅", b ] 
          
          ("MulExp", 1) -> tree `take2` \a b -> 
            grp "fraction"
              [ grp "numerator" [ a ]
              , grp "denominator" [ b ]
              ] 

          ("ExpExp", 0) -> tree `take2` \a b ->
            grp "power"
              [ a, grp "exponent" [ b ] ]
          
          ("PriExp", 0) -> tree `take1` \a ->
            grp "paren" [ a ]

          ("PriExp", 1) -> tree `take1` \a ->
            grp "negative" [ operator "-", a ]

          ("number", 0) -> tree `take2` \a b -> 
            grp "number" [ a, b ] 

          ("digit", a) -> 
            grp "digit" [text (toString a)]

          ("ident", 0) -> tree `take2` \a b -> 
            grp "ident" [ a, b ] 

          ("alpha", a) -> 
            grp "digit" [text (case Array.get a alphabeth of
              Just a -> a 
              Nothing -> "!"
            )]
          
          ("empty", _) -> 
            grp "empty" [ text "?" ]

          _ -> 
            grp "err" [ text "err" ]
--      let focusCls = if focus == id then ["ash-focus"] else []
--      in div 
--        [ classes <| [ "ash-snode"] ++ focusCls] 
--        <| Maybe.withDefault [ text "?" ] 
--        <| SyntaxTree.translate grammar tree (\str -> 
--              div 
--                [ class "ash-snode-term"] 
--                [ text str ]
--            )
  in div [ class "ash-math" ] 
      [ SyntaxTree.collect collector data ]



