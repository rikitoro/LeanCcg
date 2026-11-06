import LeanCcg.Subst

private abbrev Equations := List (Cat × Cat)

private def step (σ : Subst) (a b : Cat) (rest : Equations) :
  Option (Subst × Equations) :=
  let l := σ.apply a
  let r := σ.apply b
  if l == r then some (σ, rest)
  else
    match l, r with
    | .Var v, t | t, .Var v =>
      if Cat.occurs v t then none
      else some ((Subst.single v t).compose σ, rest)
    | .Fun d x y, .Fun e u w =>
      if d == e then some (σ, (x, u) :: (y, w) :: rest)
      else none
    | _, _ => none

private def solve (σ : Subst) : Equations → Option Subst
  | [] => some σ
  | (a, b) :: rest => do
    let (σ', next) ← step σ a b rest
    solve σ' next
partial_fixpoint

def unifyMany (eqs : Equations) : Option Subst :=
  solve Subst.empty eqs


-- 同じ変数に NP と S の両方を割り当てようとすると失敗する
#guard (unifyMany [(.Var 0 /> .Var 0, .NP /> .S)]).isNone

-- 2つの等式を介した循環も検出する
#guard (unifyMany [(.Var 0, .Var 1), (.Var 1, .S /> .Var 0)]).isNone

-- 共有した制約が、最終的な置換に反映される
#guard (unifyMany [(.Var 0, .Var 1), (.Var 1, .NP)]).any fun σ ↦
  σ.apply (.Var 0) == .NP && σ.apply (.Var 1) == .NP
