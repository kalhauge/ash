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

type Response
  = Options (List (Buffer, Int -> Int))

update : Msg -> Buffer -> Response 
update msg (Buffer {data, language} as buffer) = 
  case msg of
    Delete focus -> Options (delete focus buffer)


delete : Focus -> Buffer -> List (Buffer, Int -> Int)
delete focus buffer = 
  Debug.crash "Has" 

-- type Msg 
--   = SetData SyntaxTree

-- update : Msg -> Buffer -> Buffer
-- update msg (Buffer buffer) = 
--   case msg of
--     SetData data -> 
--       Buffer { buffer | data = data }
-- 
-- {-
-- Replaces the focused node in the buffer with the parsed content of the string
-- The alogrithm tries to be smart about it and will search for possible paths
-- in the grammar if the string did not parse in the first go. This allows for 
-- somthing like parrans to be inserted automatically.
-- -}
-- replace : Buffer -> Focus -> String -> List Msg 
-- replace {data, language} focus str = 
--   let 
--     clause = Language.clause language focus data
--     parse : Grammar.ClausePath -> Maybe SyntaxTree
--     parse (clauseid, path) = 
--       List.foldl (\i -> Maybe.map (\a -> SyntaxTree.syntax i [a]))
--         (Language.parse language clauseid str)
--         path
-- 
--     clausepaths = 
--       Language.reacableClauses language focus data 
--       
--     options = 
--       List.map parse clausepaths
--       |> Utils.compress
--       |> List.map (update data) 
--   in 
--     options
