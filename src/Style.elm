module Style exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (style, class)

import String

type alias Size =
  { width : Int
  , height : Int
  }


attr : Size -> Attribute msg
attr {width, height} = 
  style 
    [ ("width", toString width ++ "px")
    , ("height", toString height ++ "px")
    ]

classes : List String -> Attribute msg
classes = String.join " " >> class


