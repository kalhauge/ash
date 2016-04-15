module Grammar where
import Array
import Dict
import Maybe
import String

type alias Grammar     = Dict.Dict String Rule
type alias Rule        = Array.Array Alternative
type alias Alternative = List Term
type Term
  = Lex String
  | Ref String

grammar : List (String, Rule) -> Grammar
grammar = Dict.fromList

rule : List Alternative -> Rule
rule = Array.fromList

oneOf : String -> Rule
oneOf str = 
  String.toList str
  |> List.map (String.fromChar >> \x -> [ Lex x ])
  |> rule

type alias ClauseId      = String
type alias AlternativeId = Int

type alias SyntaxType = (ClauseId, AlternativeId)

get : ClauseId -> AlternativeId ->  Grammar -> Maybe Alternative
get clauseId altId grammar = 
  Dict.get clauseId grammar `Maybe.andThen` Array.get altId 
