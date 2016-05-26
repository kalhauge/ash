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
import Ash.Grammar as Grammar exposing (Grammar)

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
  | Append Focus
  | Insert Focus
  | Change Focus
  | Keep Focus

type Response
  = Options (List (Buffer, Int -> Int))

update : Msg -> Buffer -> Response 
update msg (Buffer {data, language} as buffer) = 
  let fn = case msg of
    Delete focus -> delete focus
    Replace str focus -> replace focus str
    Change focus -> change focus
    Append focus -> append focus
    Insert focus -> insert focus
    Keep focus -> keep focus
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

change : Focus -> Buffer -> List (Buffer, Focus -> Focus)
change focus (Buffer {data, language} as buffer) = 
  let
    clause = Language.clause language focus data
    grammar = Language.getGrammar language
    kinds = 
      Grammar.transitiveKinds grammar clause 
      |> List.filter (fst >> Grammar.isLexical >> not)
      |> Debug.log "kinds" 

    toChange kind =
      template grammar kind 
      |> Maybe.map (flip (put focus) buffer) 
      |> Maybe.withDefault []
  in
     List.concatMap toChange kinds

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


