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
  | Insert Focus
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
      Append str focus -> LazyOptions Append (append2 focus str buffer)
      Insert focus -> wb <| insert focus
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
keep focus (Buffer {data} as buffer)= 
  let focus' = Movement.moveSmartFocus Movement.Parrent focus data
  in 
    Command.get focus data 
    |> Maybe.map (flip (put focus') buffer)
    |> Maybe.withDefault []
    
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

append2 : Focus -> String -> Buffer -> LazyList (Buffer, Focus -> Focus)
append2 focus str (Buffer {data, language} as buffer) = 
  let
    grammar = Language.getGrammar language
    clause = Language.clause language focus data
    replaceLeftEmpty tree st = 
      let 
        empties = Debug.log "whu" <| 
          Command.empties grammar clause st 
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

(?>) a b = 
  if a then
    b ()
  else 
    Nothing

append : Focus -> Buffer -> List (Buffer, Focus -> Focus)
append focus (Buffer {data, language} as buffer) =
  let
    clause = Language.clause language focus data
    grammar = Language.getGrammar language
    kinds = 
      Grammar.transitiveKinds grammar clause 
      |> List.filter (fst >> Grammar.isLexical >> not)
      |> Debug.log "kinds" 

    append' grammar tree kind =
      Grammar.get kind grammar &> \alt -> 
      not (Grammar.isTransition alt) ?> \() -> 
      List.head (Grammar.clauseIds alt) &> \subclause -> 
      let trans = Grammar.transitiveClauses grammar subclause in
      fst tree.kind `List.member` trans ?> \() -> 
      template grammar kind &> \st -> 
      List.tail (SyntaxTree.getTerms st) &> \rest -> 
      Just (SyntaxTree.setTerms (tree :: rest) st)

    toChange tree kind =
      if not (Grammar.isLexical (fst kind)) then
        append' grammar tree kind
        |> Maybe.map (flip (put focus) buffer) 
        |> Maybe.withDefault []
      else []
  in
    case Command.get focus data of
      Just tree -> List.concatMap (toChange tree) kinds
      Nothing -> []

insert : Focus -> Buffer -> List (Buffer, Focus -> Focus)
insert focus (Buffer {data, language} as buffer) =
  let
    clause = Language.clause language focus data
    grammar = Language.getGrammar language
    kinds = 
      Grammar.transitiveKinds grammar clause 
      |> List.filter (fst >> Grammar.isLexical >> not)
      |> Debug.log "kinds" 

    insert' grammar tree kind =
      Grammar.get kind grammar &> \alt -> 
      
      not (Grammar.isTransition alt) ?> \() -> 
      
      Utils.last (Grammar.clauseIds alt) &> \subclause -> 

      let trans = Grammar.transitiveClauses grammar subclause in
      
      fst tree.kind `List.member` trans ?> \() -> 

      template grammar kind &> \st -> 
      
      Utils.init (SyntaxTree.getTerms st) &> \rest -> 
        
      Just (SyntaxTree.setTerms (rest ++ [tree]) st)

    toChange tree kind =
      if not (Grammar.isLexical (fst kind)) then
        insert' grammar tree kind
        |> Maybe.map (flip (put focus) buffer) 
        |> Maybe.withDefault []
      else []
  in
    case Command.get focus data of
      Just tree -> List.concatMap (toChange tree) kinds
      Nothing -> []


