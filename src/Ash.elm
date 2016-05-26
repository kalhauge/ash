module Ash exposing 
  ( editor
  , defaultSettings
  , Settings
  )

import Ash.Editor

import Languages.Math

type alias Settings = Ash.Editor.Settings

editor = Ash.Editor.editor

defaultSettings = 
  { after = Cmd.none
  , languages = 
    [ Languages.Math.language
    ]
  , keymaps = 
    [ ('j', "focus! child")
    , ('k', "focus! parrent")
    , ('h', "focus! prev")
    , ('l', "focus! next")

    , ('J', "focus child")
    , ('K', "focus parrent")
    , ('H', "focus prev")
    , ('L', "focus next")

    , ('r', "withInput = replace")
    , ('d', "delete")
    , ('c', "change")
    , ('a', "append")
    , ('i', "insert")
    , ('x', "keep")
    
    , (':', "withInput : command")
    ]
  }
