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
  }
