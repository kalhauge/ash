module Ash.Editor exposing (..)

{- 
This module contains the main data structure for controling the editor.
-}

import Array exposing (Array)

import Array
import Ash.SyntaxTree exposing (SyntaxTree)
import Ash.Grammar exposing (Grammar)
import Ash.Serializer exposing (Serializer)

type alias Buffer = 
  { data : SyntaxTree
  , grammar : Grammar
  }

type Mode 
  = Normal
  | Change String
  | Choose (Int, (Array.Array (Int, SyntaxTree)))

type alias View = 
  { serializer : Serializer 
  , focus : Int
  , bufferId : Int
  }

{-
The main data structure.
-}
type Editor = Editor 
  { buffers : Array Buffer
  , views : List View  
  -- this variable should also contain somthing about the layout 
  }

type EditorCmd = NoOp
