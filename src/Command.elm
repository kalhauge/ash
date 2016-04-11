module Command where

import AST exposing (..)

removeFocus : SyntaxTree -> Maybe SyntaxTree
removeFocus (SyntaxTree inner) = 
  if inner.focus then
    Just <| SyntaxTree { inner | focus = False }
  else 
    Nothing


focusOut : SyntaxTree -> SyntaxTree
focusOut (SyntaxTree inner as st) = 
  case tryUpdate inner.terms removeFocus of
    Just terms -> 
      SyntaxTree 
        { inner | focus = True, terms = List.map focusOut terms }
    Nothing -> 
      updateTerms (List.map focusOut) st

focusIn' : SyntaxTree -> SyntaxTree
focusIn' (SyntaxTree inner as st) = 
  case inner.terms of 
    SyntaxTree term :: rest -> 
      SyntaxTree 
        { inner 
        | focus = False
        , terms = 
          SyntaxTree 
            { term 
            | focus = term.focus || inner.focus 
            } :: rest
        }
    [] ->
      st

focusIn : SyntaxTree -> SyntaxTree
focusIn tree  = 
  focusIn' <| updateTerms (List.map focusIn) tree

focusNext : SyntaxTree -> SyntaxTree
focusNext tree = 
  let 
    moveNext terms = 
      case terms of
        t1 :: rest ->
          if hasFocus t1 then
            case moveNext rest of
              t2 :: rest' ->
                setFocus False t1 :: setFocus True t2 :: rest'
              [] -> [t1]
          else
            t1 :: moveNext rest
        [] -> [] 
  in updateTerms (List.map focusNext >> moveNext) tree


-- focusSmartNext: 
-- FocusSmartNext tries to fix the problem of navigating in list of
-- similiar data. There are two possible problems eigther left 
-- leaning or right leaning.
-- Right example:  
--   1| :: (2 :: 3)
--   move right and move down, if the second terms type is equal to
--   the parent.
-- Left example
--   (1 + 2|) + 3
--   if end of list and parrent has same type as granparrent
focusSmartNext : SyntaxTree -> SyntaxTree
focusSmartNext tree = 
  let 
    moveNext terms = 
      case terms of
        t1 :: rest ->
          if hasFocus t1 then
            case moveNext rest of
              t2 :: rest' ->
                let fn = 
                  if getType tree == getType t2 then
                    focusIn'
                  else
                    identity
                in setFocus False t1 :: fn (setFocus True t2) :: rest'
              [] -> [t1]
          else
            t1 :: moveNext rest
        [] -> [] 
  in updateTerms (List.map focusSmartNext >> moveNext) tree


focusPrev : SyntaxTree -> SyntaxTree
focusPrev tree =
  let 
    movePrev terms =
      case terms of
        t1 :: t2 :: rest ->
          if hasFocus t2 then
             setFocus True t1 :: setFocus False t2 :: movePrev rest
          else
            t1 :: movePrev (t2 :: rest)
        _ -> terms
  in updateTerms (movePrev >> List.map focusPrev) tree


allOf : List (Maybe a) -> Maybe (List a)
allOf maybes = 
  case maybes of
    [] ->
      Just []

    Just a :: rest -> 
      Maybe.map (\rest -> a :: rest) <| allOf rest 

    Nothing :: rest -> 
      Nothing

onlyOne : List (Maybe a) -> Maybe a
onlyOne list = 
  case list of
    [] -> 
      Nothing

    Just a :: rest ->
      case Maybe.oneOf rest of 
        Just _ -> Nothing
        Nothing -> Just a

    Nothing :: rest ->
      onlyOne rest

deleteFocus : SyntaxTree -> Maybe SyntaxTree
deleteFocus (SyntaxTree inner as tree) = 
  if hasFocus tree then
    Nothing
  else
    let 
      terms = List.map deleteFocus inner.terms 
    in
      case allOf terms of
        Just terms' -> 
          Just (setTerms terms' tree)
        Nothing -> 
          Maybe.map (setFocus True) (onlyOne terms)



