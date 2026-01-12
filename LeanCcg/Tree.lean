import LeanCcg.Utility
import LeanCcg.Rules

/- # 導出木 (Tree) の定義 -/

inductive Tree : Type
  | leaf (tok : Token) (c : Cat)
  | branch (r : BinaryRule) (c : Cat) (lt rt : Tree)
  | unary (r : UnaryRule) (c : Cat) (t : Tree)


def Tree.cat : Tree → Cat
  | .leaf _ c => c
  | .branch _ c _ _ => c
  | .unary _ c _ => c

---

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
    pre : Nat → String
    | 0 => ""
    | n + 1 => "│".replicateStr n ++ "├"

def Tree.toString (t : Tree) : String :=
  "\n" ++ t.toStringAux 0

instance : ToString Tree where
  toString := Tree.toString
