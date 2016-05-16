module Ash exposing 
  ( editor
  , defaultSettings
  , Settings
  )

import Ash.Editor

type alias Settings = Ash.Editor.Settings

editor = Ash.Editor.editor

defaultSettings = 
  { after = Cmd.none
  }
