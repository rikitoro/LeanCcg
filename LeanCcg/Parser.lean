import LeanCcg.Chart
import LeanCcg.Lexicon

def parseCCG (toks : List Token) (lexicon : Token → List Cat) : List Tree :=
  let chart := fillChart toks lexicon
  let len := toks.length
  chart.lookup len 0


#eval parseCCG ["Keats", "eats", "an", "apple"] lexicon

def parse (sentence : String) (lexicon : Token → List Cat := lexicon): List Tree :=
  let toks : List Token := sentence.toTokens
  parseCCG toks lexicon

#eval parse "Keats eats an apple"

#eval parse "wrote"
