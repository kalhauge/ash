module Ash.Buffer exposing 
  ( Buffer(..)
  , new
  , onData
  , setData

  , Msg (..)
  , Response (..)
  , update
  )

import Ash.Language as Language exposing (Language)
import Ash.SyntaxTree as SyntaxTree exposing (SyntaxTree, Focus)
import Ash.Command as Command
import Ash.Grammar as Grammar exposing (Grammar)

import Utils

type Buffer = Buffer
  { data : SyntaxTree
  , language : Language
  }

new : Language -> Buffer 
new language = 
  Buffer 
    { data = SyntaxTree.empty
    , language = language
    }

onData : (SyntaxTree -> a) -> Buffer -> a
onData fn (Buffer {data}) =
  fn data

setData : SyntaxTree -> Buffer -> Buffer
setData data (Buffer b) = Buffer { b | data = data } 

{- Actions -} 

type Msg 
  = Delete Focus
  | Replace String Focus
  | Change Focus

type Response
  = Options (List (Buffer, Int -> Int))

update : Msg -> Buffer -> Response 
update msg (Buffer {data, language} as buffer) = 
  let fn = case msg of
    Delete focus -> delete focus
    Replace str focus -> replace focus str
    Change focus -> change focus
  in Options (fn buffer)

lift : 
  (SyntaxTree -> Maybe (SyntaxTree, Focus -> Focus)) 
  -> Buffer 
  -> List (Buffer, Focus -> Focus) 
lift fn (Buffer {data} as buffer) = 
  fn data
  |> Maybe.map (\(st, ff) -> (setData st buffer, ff))
  |> Utils.maybeToList

delete : Focus -> Buffer -> List (Buffer, Focus -> Focus)
delete focus = 
  lift (Command.delete focus)
    
put : Focus -> SyntaxTree -> Buffer -> List (Buffer, Focus -> Focus)
put focus st = 
  lift 
  <| Command.replace focus (always st)

applyPath : List Grammar.SyntaxKind -> SyntaxTree -> SyntaxTree
applyPath = 
  flip <| List.foldl (\i a -> SyntaxTree.syntax i [a]) 

replace : Focus -> String -> Buffer -> List (Buffer, Focus -> Focus)
replace focus str (Buffer {data, language} as buffer) =
  let 
    clause = Debug.log "parse" <| Language.clause language focus data
    grammar = Language.getGrammar language
    
    parse : Grammar.ClausePath -> Maybe SyntaxTree
    parse (clauseid, path) = 
      Language.parse language clauseid str
      |> Maybe.map (applyPath path) 

    replace = 
      Command.trim grammar
      >> flip (put focus) buffer

  in case parse (clause, []) of
    Just new -> replace new
    Nothing -> 
      Debug.log "chsle" (List.filter (snd >> List.isEmpty >> not) (Grammar.reachableClauses grammar clause))
      |> List.concatMap (parse >> Maybe.map replace >> Maybe.withDefault [])

change : Focus -> Buffer -> List (Buffer, Focus -> Focus)
change focus (Buffer {data, language} as buffer) = 
  let
    clause = Language.clause language focus data
    grammar = Language.getGrammar language
    kinds = Debug.log "kinds" <| Grammar.reachableKinds grammar clause 

    toChange (kind, path) =
      if List.isEmpty path && not (Grammar.isLexical (fst kind)) then
        case Grammar.get kind grammar of
          Just alt -> 
            if not (Grammar.isTransition alt) then
              let
                st = SyntaxTree.syntax kind 
                  (List.map (always SyntaxTree.empty) <| Grammar.clauseIds alt)
              in put focus (applyPath path st) buffer
            else []
          Nothing -> 
            []
      else 
        []
  in
     List.concatMap toChange kinds




