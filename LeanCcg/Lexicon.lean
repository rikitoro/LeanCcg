import LeanCcg.Util
import LeanCcg.Cat

def lexicon : Token → List Cat
  | "Keats"   => [.NP]
  | "eats"    => [.S \> .NP /> .NP]
  | "cooks"   => [.S \> .NP /> .NP]
  | "an"      => [.NP /> .N]
  | "apple"   => [.N]
  | "apples"  => [.NP]
  | "might"   => [(.S \> .NP) /> (.S \> .NP)]
  | "I"       => [.NP, .S /> (.S \> .NP)]
  | "think"   => [.S \> .NP /> .S]
  | "that"    => [.S /> .S, .NP /> .N, .N, .N \> .N /> (.S /> .NP)]
  | "boy"     => [.N]
  | "wrote"   => [.S \> .NP /> .NP]
  | "is"      => [.S \> .NP /> (.S \> .NP)]
  | "wrong"   => [.S \> .NP /> (.S \> .NP)]
  | _         => []
