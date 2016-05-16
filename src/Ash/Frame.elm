module Ash.Frame exposing (..)
import Ash.Serializer exposing (Serializer)

import Ash.SyntaxTree exposing (SyntaxTree)

import Array exposing (Array)

type Mode 
  = Normal
  | Change String
  | Choose (Int, (Array (Int, SyntaxTree)))

type alias Frame = 
  { serializer : Serializer 
  , focus : Int
  , bufferId : Int
  , mode : Mode
  }


