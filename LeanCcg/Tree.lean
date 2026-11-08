import LeanCcg.Utility
import LeanCcg.Rules

/- # 導出木 (Tree) の定義 -/

inductive Tree : Type
  | leaf (tok : Token) (c : Cat)
  | branch (r : BinaryRule) (c : Cat) (lt rt : Tree)
  | unary (r : UnaryRule) (c : Cat) (t : Tree)

namespace Tree

def cat : Tree → Cat
  | .leaf _ c => c
  | .branch _ c _ _ => c
  | .unary _ c _ => c

/-- 根から葉まで、すべての範疇に同じ変換を適用 -/
def mapCats (f : Cat → Cat) : Tree → Tree
  | .leaf tok c => .leaf tok (f c)
  | .branch r c lt rt =>
    .branch r (f c) (mapCats f lt) (mapCats f rt)
  | .unary r c t => .unary r (f c) (mapCats f t)

def applySubst (σ : Subst) (t : Tree) : Tree :=
  t.mapCats σ.apply

def renameVars (rename : VarId → VarId) (t : Tree) : Tree :=
  t.mapCats (Cat.renameVars rename)

/-- 根とすべての子の変数IDよりも大きな値を返す -/
def nextVarId : Tree → VarId
  | .leaf _ c => c.nextVarId
  | .branch _ c lt rt =>
    max c.nextVarId (max lt.nextVarId rt.nextVarId)
  | .unary _ c t => max c.nextVarId t.nextVarId


---

private def toStringAux (n : Nat) : Tree → String
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

def toString (t : Tree) : String :=
  "\n" ++ t.toStringAux 0

end Tree

instance : ToString Tree where
  toString := Tree.toString
