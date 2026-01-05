import LeanCcg.Chart
import LeanCcg.Lexicon

def parseCCG  (lexicon : Token → List Cat) (toks : List Token) : List Tree :=
  let chart := fillChart toks lexicon
  let len := toks.length
  chart.lookup len 0


#eval parseCCG lexicon1 ["Keats"]
#eval parseCCG lexicon1 ["Keats", "eats", "an", "apple"]

def parse  (lexicon : Token → List Cat) (sentence : String): List Tree :=
  let toks : List Token := sentence.toTokens
  parseCCG lexicon toks

#eval parse lexicon1 "Keats"
#eval parse lexicon1 "Keats eats an apple"

#eval parse lexicon2 "I think that that that that that boy wrote is wrong"
