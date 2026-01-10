import LeanCcg.Parsing

def lexicon1 : Token → List Cat
  | "Keats"   => [.NP]
  | "eats"    => [.S \> .NP /> .NP]
  | "cooks"   => [.S \> .NP /> .NP]
  | "an"      => [.NP /> .N]
  | "apple"   => [.N]
  | "apples"  => [.NP]
  | _         => []

#eval parse lexicon1 "Keats eats apples"
#eval parse lexicon1 "Keats eats an apple"

def lexicon2 : Token → List Cat
  | "I"       => [.S /> (.S \> .NP)]
  | "think"   => [.S \> .NP /> .S,]
  | "that"    => [.S /> .S, .NP /> .N, .N, .N \> .N /> (.S /> .NP)]
  | "boy"     => [.N]
  | "wrote"   => [.S \> .NP /> .NP]
  | "is"      => [.S \> .NP /> (.S \> .NP)]
  | "wrong"   => [.S \> .NP]
  | _ => []

#eval parse lexicon2 "I think that that that that that boy wrote is wrong"

def lexicon3 : Token → List Cat
  | "Keats"   => [.NP]
  | "will"    => [.S \> .NP /> (.S \> .NP)]
  | "file"    => [.S \> .NP /> .NP]
  | "without" => [.S \> .NP \> (.S \> .NP) /> (.S \> .NP)]
  | "reading" => [.S \> .NP /> .NP]
  | "these"   => [.NP /> .NP]
  | "articles"  => [.NP]
  | _ => []


#eval parse lexicon3 "file without reading"
#eval parse lexicon3 "Keats will file without reading these articles"
