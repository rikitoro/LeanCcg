import LeanCcg.Unify

/- # CCGの組合せ規則 (BinaryRule, UnaryRule) -/

/- ## 組合せ規則の適用 -/

/-- 規則適用で得た範疇と入力の範疇に関する置換 -/
structure RuleResult where
  cat   : Cat
  subst : Subst


-- ### Binary Rules


/-- 順関数適用 [>] : 左辺の引数範疇と右辺の範疇を単一化する -/
def fappWithSubst : Cat → Cat → Option RuleResult
  | .Fun .Fwd result arg, right => do
    let σ ← unifyMany [(arg, right)]
    return { cat := σ.apply result, subst := σ }
  | _, _ => none

/-- 逆向き関数適用 [<] : 右辺の引数範疇と左辺の範疇を単一化する -/
def bappWithSubst : Cat → Cat → Option RuleResult
  | left, .Fun .Bwd result arg => do
    let σ ← unifyMany [(arg, left)]
    return { cat := σ.apply result, subst := σ }
  | _, _ => none

/-- Forward functional application
  [>] x/y y ⟹ x -/
def fapp : Cat → Cat → Option Cat
  | .Fun .Fwd x y, y' => if y == y' then some x else none
  | _, _  => none

/-- Backward functional application
  [<] y x\y ⟹ x -/
def bapp : Cat → Cat → Option Cat
  | y', .Fun .Bwd x y => if y == y' then some x else none
  | _, _ => none

/-- Generalized forward functional composition
  [>B] x/y y/$/w ⟹ x/$/w -/
def fcompGen : Cat → Cat → Option Cat
  | .Fun .Fwd x y, .Fun .Fwd y' z =>
    if y == y' then
      some (x /> z)
    else
      match fcompGen (x /> y) y' with
      | some x' => some (x' /> z)
      | none => none
  | _, _ => none

/-- Generalized backward functional composition
  [<B] y\$\z x\y ⟹ x\$\z -/
def bcompGen : Cat → Cat → Option Cat
  | .Fun .Bwd y' z, .Fun .Bwd x y =>
    if y == y' then
      some (x \> z)
    else
      match bcompGen y' (x \> y) with
      | some x' => some (x' \> z)
      | none => none
  | _, _ => none

/-- Forward crossed substitution
  [>Sx] x/y\z y\z ⟹ x\z -/
def fcross : Cat → Cat → Option Cat
  | .Fun .Bwd (.Fun .Fwd x y) z, .Fun .Bwd y' z' =>
    if y == y' && z == z' then
      some (x \> z)
    else
      none
  | _, _ => none

/-- Backward crossed substitution
  [>Sx] x/y\z y\z ⟹ x\z -/
def bcross : Cat → Cat → Option Cat
  | .Fun .Fwd y' z', .Fun .Fwd (.Fun .Bwd x y) z  =>
    if y == y' && z == z' then
      some (x /> z)
    else
      none
  | _, _ => none

-- ### Unary Rules

/-- Forward Type rising
  [>T] x ⟹ t/(t\x) : 今回は x = NP, t = S に限定する -/
def fraise (x : Cat := .NP) (t : Cat := .S) : Cat → Option Cat
  | .NP => some (t /> (t \> x))
  | _ => none

/-- Backward Type rising
  [<T] x ⟹ t\ (t/x) : 今回は x = NP, t = S に限定する -/
def braise (x : Cat := .NP) (t : Cat := .S) : Cat → Option Cat
  | .NP => some (t \> (t /> x))
  | _ => none


/- ## 組合せ規則の一覧 -/

inductive BinaryRule : Type
  | Fa  -- forward app [>]
  | Ba  -- backward app [<]
  | Fc  -- forward comp gen [>B]
  | Bc  -- backward comp gen [<B]
  | Fx  -- forward crossed subst [>Sx]
  | Bx  -- backward crossed subst [<Sx]

inductive UnaryRule : Type
  | Ft  -- forward type raising [>T]
  | Bt  -- backward type raising [<T]

def binaryRules : List BinaryRule := [.Fa, .Ba, .Fc, .Bc, .Fx, .Bx]
def unaryRules  : List UnaryRule  := [.Ft, .Bt]

---

def BinaryRule.toString : BinaryRule → String
  | .Fa => "[>]"
  | .Ba => "[<]"
  | .Fc => "[>B]"
  | .Bc => "[<B]"
  | .Fx => "[>Sx]"
  | .Bx => "[<Sx]"

def UnaryRule.toString : UnaryRule → String
  | .Ft => "[>T]"
  | .Bt => "[<T]"

instance : ToString BinaryRule where
  toString := BinaryRule.toString

instance : ToString UnaryRule where
  toString := UnaryRule.toString

/- ## Rule との対応付け -/

def BinaryRule.applyBinary : BinaryRule → Cat → Cat → Option Cat
  | .Fa => fapp
  | .Ba => bapp
  | .Fc => fcompGen
  | .Bc => bcompGen
  | .Fx => fcross
  | .Bx => bcross

/-- 順関数適用は単一化、他の規則の結果には空置換を付ける -/
def BinaryRule.applyBinaryWithSubst
  (r : BinaryRule) (lc rc : Cat) : Option RuleResult :=
  match r with
  | .Fa => fappWithSubst lc rc
  | _ => (r.applyBinary lc rc).map fun c ↦
    { cat := c, subst := Subst.empty }


def UnaryRule.applyUnary : UnaryRule → Cat → Option Cat
  | .Ft => fraise
  | .Bt => braise


----

private def testAndCat : Cat :=
  let x := Cat.Var 0
  x \> x /> x

-- and apples : X が NP に決まる
#guard (fappWithSubst testAndCat .NP).any fun r ↦
  r.cat == .NP \> .NP && r.subst.apply (.Var 0) == .NP

-- 右辺が動詞句 S\NP の場合 : X は S\NP に決まる
#guard
  let vp : Cat := .S \> .NP
  (fappWithSubst testAndCat vp).any fun r ↦
    r.cat == (vp \> vp) && r.subst.apply (.Var 0) == vp

-- 右辺に変数が来た場合
#guard (fappWithSubst (.S /> .NP) (.Var 1)).any fun r ↦
  r.cat == .S && r.subst.apply (.Var 1) == .NP

-- 引数の不一致
#guard (fappWithSubst (.S /> .NP) .N).isNone

-- スラッシュの向きの違い
#guard (fappWithSubst (.S \> .NP) .NP).isNone

-- 逆向き関数適用 : 右辺の X\X の X が NP に決まる
#guard (bappWithSubst .NP (.Var 0 \> .Var 0)).any fun r ↦
  r.cat == .NP && r.subst.apply (.Var 0) == .NP

-- 関数型の範疇も引数にできる
#guard
  let vp : Cat := .S \> .NP
  (bappWithSubst vp (.Var 1 \> .Var 1)).any fun r ↦
    r.cat == vp && r.subst.apply (.Var 1) == vp

-- 左辺の変数も右辺の関数が要求する引数範疇に具体化する
#guard (bappWithSubst (.Var 1) (.S \> .NP)).any fun r ↦
  r.cat == .S && r.subst.apply (.Var 1) == .NP

-- 引数の不一致
#guard (bappWithSubst .N (.S \> .NP)).isNone

-- スラッシュの向きの違い
#guard (bappWithSubst .NP (.S /> .NP)).isNone

-- 循環する制約 X0 = X0/S は拒否
#guard (bappWithSubst (.Var 0) (.NP \> (.Var 0 /> .S))).isNone
