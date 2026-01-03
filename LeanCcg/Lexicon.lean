import LeanCcg.Util
import LeanCcg.Cat

def lexicon : String → List Cat
  | "Keats"   => [.NP]
  | "eats"    => [.S \> .NP /> .NP]
  | "cooks"   => [.S \> .NP /> .NP]
  | "an"      => [.NP /> .N]
  | "apple"   => [.N]
  | "apples"  => [.NP]
  | "might"   => [(.S \> .NP) /> (.S \> .NP)]
  | _         => []
