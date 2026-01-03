import LeanCcg.Chart

def parseCCG (toks : List String) : List Tree :=
  let chart := fillChart toks
  let len := toks.length
  chart.lookup len 0


#eval parseCCG ["Keats", "eats", "an", "apple"]

def parse (sentence : String) : List Tree :=
  let toks : List String := sentence.toTokens
  parseCCG toks

#eval parse "Keats eats an apple"
