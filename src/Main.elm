port module Main exposing (..)

import Ash exposing (editor, defaultSettings)

port onload : String -> Cmd msg

main = 
  editor 
    { defaultSettings |
      after = onload "hi"
    }
    
