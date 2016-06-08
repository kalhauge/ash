module Ash.Buffer exposing 
  ( Buffer(..)
  , new
  , onData
  , setData

  , getLanguage

  , findEmpty

  , Msg (..)
  , Response (..)
  , update
  )

import Ash.Language as Language exposing (Language)
import Ash.SyntaxTree as SyntaxTree exposing (SyntaxTree, Focus)
import Ash.Command as Command
import Ash.Movement as Movement
import Ash.Parser as Parser
import Ash.Grammar as Grammar exposing (Grammar)

import Lazy.List as LazyList exposing (LazyList)
import Utils exposing ((&>))

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

getLanguage (Buffer {language}) = language

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
  | Change String Focus
  | Append String Focus
  | Insert String Focus
  | Keep Focus

type Response
  = Options (List (Buffer, Int -> Int))
  | LazyOptions (String -> Focus -> Msg) (LazyList (Buffer, Int -> Int))

update : Msg -> Buffer -> Response 
update msg (Buffer {data, language} as buffer) = 
  let 
    wb f = Options (f buffer)
  in 
    case msg of
      Delete focus -> wb <| delete focus
      Replace str focus -> wb <| replace focus str
      Change str focus -> LazyOptions Change (change focus str buffer)
      Append str focus -> LazyOptions Append (append focus str buffer)
      Insert str focus -> LazyOptions Insert (insert focus str buffer)
      Keep focus -> wb <| keep focus

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

keep : Focus -> Buffer -> List (Buffer, Focus -> Focus)
keep focus (Buffer {data, language} as buffer)= 
  let 
    parrents = SyntaxTree.collectPathIds focus data
    grammar = Language.getGrammar language
    clause = Language.clause language focus data
    keeper focus'= 
      if clause `List.member` 
         Grammar.transitiveClauses grammar 
           (Language.clause language focus' data)
      then 
        Command.get focus data 
        |> Maybe.map (flip (put focus') buffer)
        |> Maybe.withDefault []
      else
        []
  in
    case List.tail (List.reverse parrents) of
      Just focuci ->  
        List.concatMap keeper focuci 
      Nothing -> []
    
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
    clause = Language.clause language focus data
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

template : Grammar -> Grammar.SyntaxKind -> Maybe SyntaxTree
template grammar kind =
  Grammar.get kind grammar `Maybe.andThen` \alt -> 
  if not (Grammar.isTransition alt) then
    Just 
      <| SyntaxTree.syntax kind 
      <| List.map (always SyntaxTree.empty) 
      <| Grammar.clauseIds alt
  else Nothing

suggestions : 
    Focus 
    -> String 
    -> Buffer
    -> LazyList SyntaxTree
suggestions focus str (Buffer {data, language} as buffer) =
  let
    clause = Language.clause language focus data
    grammar = Language.getGrammar language

    suggest : Grammar.ClauseId -> LazyList SyntaxTree
    suggest clauseid = 
      Parser.suggest grammar clauseid str
      |> LazyList.map (Command.trim grammar)
    
    suggestWithParen : (Grammar.ClauseId, Grammar.SyntaxKind) -> LazyList SyntaxTree
    suggestWithParen (cid, kind) = 
      LazyList.map (applyPath [kind]) <| suggest cid 

    simple = suggest clause 
  in
    if not <| LazyList.isEmpty simple then
      simple
    else
      Grammar.parens grammar clause
      |> LazyList.fromList
      |> LazyList.flatMap suggestWithParen 

replaceInBuffer : 
  Focus 
  -> Buffer 
  -> SyntaxTree
  -> LazyList (Buffer, Focus -> Focus) 
replaceInBuffer focus buffer = 
  LazyList.fromList << flip (put focus) buffer

change : Focus -> String -> Buffer -> LazyList (Buffer, Focus -> Focus)
change focus str buffer =
  suggestions focus str buffer
  |> LazyList.flatMap (replaceInBuffer focus buffer)

append : Focus -> String -> Buffer -> LazyList (Buffer, Focus -> Focus)
append focus str (Buffer {data, language} as buffer) = 
  let
    grammar = Language.getGrammar language
    clause = Language.clause language focus data
    replaceLeftEmpty tree st = 
      let 
        empties = Command.empties grammar clause st 
        replaceEmpty (id, clauseid) = 
          if fst tree.kind `List.member` 
             Grammar.transitiveClauses grammar clauseid 
          then
            Maybe.map fst <| Command.replace id (always <| 
              Debug.log "tree" tree) (Debug.log "st" st)
          else
            Nothing
      in 
        case Utils.oneOfMap replaceEmpty empties of
          Just x -> LazyList.singleton x
          Nothing -> LazyList.empty

  in
    case Command.get focus data of
      Just tree -> 
        suggestions focus str buffer
        |> LazyList.flatMap (replaceLeftEmpty tree)
        |> LazyList.flatMap (replaceInBuffer focus buffer)

      Nothing -> 
        LazyList.empty


insert : Focus -> String -> Buffer -> LazyList (Buffer, Focus -> Focus)
insert focus str (Buffer {data, language} as buffer) = 
  let
    grammar = Language.getGrammar language
    clause = Language.clause language focus data
    replaceLeftEmpty tree st = 
      let 
        empties = List.reverse <| Command.empties grammar clause st 
        replaceEmpty (id, clauseid) = 
          if fst tree.kind `List.member` 
             Grammar.transitiveClauses grammar clauseid 
          then
            Maybe.map fst <| Command.replace id (always <| 
              Debug.log "tree" tree) (Debug.log "st" st)
          else
            Nothing
      in 
        case Utils.oneOfMap replaceEmpty empties of
          Just x -> LazyList.singleton x
          Nothing -> LazyList.empty

  in
    case Command.get focus data of
      Just tree -> 
        suggestions focus str buffer
        |> LazyList.flatMap (replaceLeftEmpty tree)
        |> LazyList.flatMap (replaceInBuffer focus buffer)

      Nothing -> 
        LazyList.empty


