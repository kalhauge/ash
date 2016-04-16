module Parser where

import List exposing (..)
import Array
import String

import Utils exposing (..)
import Grammar exposing (..)
import AST exposing (..)

parse : Grammar -> ClauseId -> String -> Maybe SyntaxTree
parse grammar entry = 
  let 
    parseRule ruleName str = 
      getRule ruleName grammar `Maybe.andThen` 
        (\rule ->
          List.indexedMap (,) (Array.toList rule)
            |> parseRules [] str ruleName
        )
    
    parseRules rec str ruleName alts =
      let 
        continue alt rest = 
          case parseAlt str ruleName alt of 
            Nothing -> 
              parseRules rec str ruleName rest
            Just (result, str')  -> 
              let val = 
                    oneOfMap 
                      (parseLeftAlt str' ruleName result)             
                      (List.reverse rec)
              in case val of
                 Nothing -> Just (result, str')
                 a -> a
      in
        case alts of 
          ((i, terms) as alt) :: rest -> 
            case terms of 
              Ref ruleName' :: restTerms -> 
                -- Check for left recursion
                if ruleName' == ruleName then
                  parseRules ((i, restTerms) :: rec) str ruleName rest
                else 
                  continue alt rest
              _ -> 
                continue alt rest
                
          [] -> 
            Nothing
    
    parseLeftAlt str ruleName first (i, alt) = 
      Debug.log ("LeftAlt: " ++ ruleName ++ " " ++ toString i ++ ": '" ++ str ++ "'")
      ( parseTerms str alt
        |> Maybe.map (\(terms, str') -> 
            (syntax ruleName i (first :: terms), str')
          )
      )
 
    parseAlt str ruleName (i, alt) = 
      Debug.log ("Alt: " ++ ruleName ++ " " ++ toString i ++ ": '" ++ str ++ "'")
        ( parseTerms str alt
          |> Maybe.map (\(terms, str') -> 
              (syntax ruleName i terms, str')
            )
        )
  
    parseTerms str alt =
      case alt of 
        Lex lex :: rest ->
          if String.startsWith lex str then
            parseTerms (String.dropLeft (String.length lex) str) rest
          else
            Nothing 
        Ref rule :: rest -> 
          case parseRule rule str of
            Just (tree, str') -> 
              case parseTerms str' rest of
                Just (terms, str'') -> 
                  Just (tree :: terms, str'')
                Nothing -> Nothing
            Nothing -> Nothing
        [] -> 
          Just ([], str)
  in 
    parseRule entry 
      >> Maybe.map fst



