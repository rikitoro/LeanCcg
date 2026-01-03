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
  | _         => []
