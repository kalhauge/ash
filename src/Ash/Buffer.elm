module Ash.Buffer exposing 
  ( Buffer(..)
  , new
  , onData
  , setData

  , findEmpty

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

{- Finds next empty syntax tree -} 
findEmpty : Buffer -> Maybe Int
findEmpty (Buffer {data}) = 
  SyntaxTree.first (\i -> SyntaxTree.isEmpty >> Utils.maybeIf i) data


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
    
    parse : Grammar.ClauseId -> Maybe SyntaxTree
    parse clauseid = 
      Language.parse language clauseid str

    parseP : (Grammar.ClauseId, Grammar.SyntaxKind) -> Maybe SyntaxTree
    parseP (cid, kind) = 
      Maybe.map (applyPath [kind]) <| Language.parse language cid str

    parrans : Grammar.SyntaxKind -> Maybe (Grammar.ClauseId, Grammar.SyntaxKind)
    parrans kind = 
      Grammar.get kind grammar `Maybe.andThen` \alt -> 
      if Grammar.isTransition alt then
        Nothing
      else 
        case Grammar.clauseIds alt of
          [subid] -> Just (subid, kind)
          _ -> Nothing

    replace = 
      Command.trim grammar
      >> flip (put focus) buffer

  in case parse clause of
    Just new -> replace new
    Nothing -> 
      Grammar.transitiveKinds grammar clause
      |> List.map parrans
      |> Utils.compress
      |> List.concatMap (parseP >> Maybe.map replace >> Maybe.withDefault [])

change : Focus -> Buffer -> List (Buffer, Focus -> Focus)
change focus (Buffer {data, language} as buffer) = 
  let
    clause = Language.clause language focus data
    grammar = Language.getGrammar language
    kinds = Debug.log "kinds" <| Grammar.transitiveKinds grammar clause 

    toChange kind =
      if not (Grammar.isLexical (fst kind)) then
        case Grammar.get kind grammar of
          Just alt -> 
            if not (Grammar.isTransition alt) then
              let
                st = SyntaxTree.syntax kind 
                  (List.map (always SyntaxTree.empty) <| Grammar.clauseIds alt)
              in put focus st buffer
            else []
          Nothing -> 
            []
      else 
        []
  in
     List.concatMap toChange kinds




