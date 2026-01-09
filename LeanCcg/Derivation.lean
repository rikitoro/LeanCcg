import LeanCcg.Util
import LeanCcg.Syntax
import LeanCcg.Rule


/- ## 導出木 Tree の定義 -/

inductive Tree : Type
  | leaf (tok : Token) (c : Cat)
  | branch (r : Rule) (c : Cat) (lt rt : Tree)
  | unary (r : Rule) (c : Cat) (t : Tree) -- type raing 用


def Tree.cat : Tree → Cat
  | .leaf _ c => c
  | .branch _ c _ _ => c
  | .unary _ c _ => c


private def Tree.toStringAux (n : Nat) : Tree → String
  | .leaf t c =>
    pre n ++
    s!"{c} \"{t}\""
  | .branch r c lt rt =>
    pre n ++ s!"{c} {r}\n" ++
    toStringAux (n + 1) lt ++ "\n" ++
    toStringAux (n + 1) rt
  | .unary r c t =>
    pre n ++ s!"{c} {r}\n" ++
    toStringAux (n + 1) t
  where
    pre n := "| ".replicateStr n

def Tree.toString (t : Tree) : String :=
  "\n" ++ t.toStringAux 0

instance : ToString Tree where
  toString := Tree.toString
