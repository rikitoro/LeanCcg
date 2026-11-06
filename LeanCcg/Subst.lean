import LeanCcg.Category

/-- 範疇変数に対する有限置換 -/
structure Subst where
  bindings : List (VarId × Cat) := []

namespace Subst

def empty : Subst := {}

def single (v : VarId) (c : Cat) : Subst :=
  { bindings := [(v, c)] }

def lookup (σ : Subst) (v : VarId) : Option Cat :=
  σ.bindings.lookup v

/-- 範疇に同時置換を1回適用する -/
def apply (σ : Subst) : Cat → Cat
  | .Var v      => (σ.lookup v).getD (.Var v)
  | .Fun d x y  => .Fun d (apply σ x) (apply σ y)
  | c           => c

/-- before, after の順に適用する置換 -/
def compose (after before : Subst) : Subst :=
  let updated := before.bindings.map fun (v, c) ↦ (v, after.apply c)
  let extra := after.bindings.filter fun (v, _) ↦ (before.lookup v).isNone
  { bindings := updated ++ extra }

end Subst

private def andCat : Cat :=
  let x := Cat.Var 0
  x \> x /> x

-- X0 ↦ NP : 3か所すべてが NP になる
#guard (Subst.single 0 .NP).apply andCat == .NP \> .NP /> .NP
-- #eval (Subst.single 0 .NP).apply andCat -- NP\NP/NP

-- X0 ↦ S\NP : andCat を動詞句にも使える
#guard
  let vp : Cat := .S \> .NP
  (Subst.single 0 vp).apply andCat == vp \> vp /> vp
-- #eval
--   let vp : Cat := .S \> .NP
--   (Subst.single 0 vp).apply andCat -- S\NP\(S\NP)/(S\NP)

-- X0 への代入は X1 に影響しない
#guard (Subst.single 0 .NP).apply (.Var 1) == .Var 1


#guard
  let first  := Subst.single 0 (.Var 1) -- X0 ↦ X1
  let second := Subst.single 1 .NP      -- X1 ↦ NP
  let σ := second.compose first
  σ.apply (.Var 0) == .NP && σ.apply (.Var 1) == .NP
