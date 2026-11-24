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

/-- 後ろ向き関数適用 [<] : 右辺の引数範疇と左辺の範疇を単一化する -/
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

/-- 一般化前向き関数合成 [>B] 浅い段から単一化を試し、最初の成功を返す -/
def fcompGenWithSubst : Cat → Cat → Option RuleResult
  | .Fun .Fwd x y, .Fun .Fwd y' z =>
    match unifyMany [(y, y')] with
    | some σ => some { cat := σ.apply (x /> z), subst := σ }
    | none => do
      let r ← fcompGenWithSubst (x /> y) y'
      -- 内側の合成で決まった置換を外側に残る引数にも適用する
      return { cat := r.cat /> r.subst.apply z, subst := r.subst }
  | _, _ => none

/-- 一般化後向き関数合成 [<B] -/
def bcompGenWithSubst : Cat → Cat → Option RuleResult
  | .Fun .Bwd y' z, .Fun .Bwd x y =>
    match unifyMany [(y, y')] with
    | some σ => some { cat := σ.apply (x \> z), subst := σ}
    | none => do
      let r ← bcompGenWithSubst y' (x \> y)
      return { cat := r.cat \> r.subst.apply z, subst := r.subst }
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

/-- 前向き交差置換 [>Sx] -/
def fcrossWithSubst : Cat → Cat → Option RuleResult
  | .Fun .Bwd (.Fun .Fwd x y) z, .Fun .Bwd y' z' => do
    let σ ← unifyMany [(y, y'), (z, z')]
    return { cat := σ.apply (x \> z), subst := σ }
  | _, _ => none

/-- 後向き交差置換 [<Sx] -/
def bcrossWithSubst : Cat → Cat → Option RuleResult
  | .Fun .Fwd y' z', .Fun .Fwd (.Fun .Bwd x y) z => do
    let σ ← unifyMany [(y, y'), (z, z')]
    return { cat := σ.apply (x /> z), subst := σ }
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

/-- すべての二項規則で単一化を行い、範疇と置換を返す -/
def BinaryRule.applyBinaryWithSubst
  (r : BinaryRule) (lc rc : Cat) : Option RuleResult :=
  match r with
  | .Fa => fappWithSubst lc rc
  | .Ba => bappWithSubst lc rc
  | .Fc => fcompGenWithSubst lc rc
  | .Bc => bcompGenWithSubst lc rc
  | .Fx => fcrossWithSubst lc rc
  | .Bx => bcrossWithSubst lc rc


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

/- # 後ろ向き関数適用の単一化 -/

-- 後ろ向き関数適用 : 右辺の X\X の X が NP に決まる
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

-- 引数の不一致、スラッシュの向きの違いは拒否
#guard (bappWithSubst .N (.S \> .NP)).isNone
#guard (bappWithSubst .NP (.S /> .NP)).isNone

-- 同じ変数へ矛盾した制約、循環する制約は拒否
#guard (bappWithSubst (.NP /> .S) (.S \> (.Var 0 /> .Var 0))).isNone
#guard (bappWithSubst (.Var 0) (.NP \> (.Var 0 /> .S))).isNone

-- パーサが使う規則振り分けでも Ba の単一化を行う
#guard (BinaryRule.Ba.applyBinaryWithSubst .NP (.Var 0 \> .Var 0)).any fun r ↦
  r.cat == .NP && r.subst.apply (.Var 0) == .NP

/- # 一般化関数合成の単一化 -/

-- 変数を含まない通常の関数合成
#guard (fcompGenWithSubst (.S /> .NP) (.NP /> .N)).any fun r ↦
  r.cat == .S /> .N
#guard (bcompGenWithSubst (.NP \> .N) (.S \> .NP)).any fun r ↦
  r.cat == .S \> .N

-- X/X と X\X の結果側の X にも置換が反映
#guard (fcompGenWithSubst (.Var 0 /> .Var 0) (.NP /> .N)).any fun r ↦
  r.cat == .NP /> .N && r.subst.apply (.Var 0) == .NP
#guard (bcompGenWithSubst (.NP \> .N) (.Var 0 \> .Var 0)).any fun r ↦
  r.cat == .NP \> .N && r.subst.apply (.Var 0) == .NP

-- 合成後に残る引数に含まれる変数にも置換を適用
#guard (fcompGenWithSubst (.S /> .NP) (.Var 0 /> .Var 0)).any fun r ↦
  r.cat == .S /> .NP && r.subst.apply (.Var 0) == .NP
#guard (bcompGenWithSubst (.Var 0 \> .Var 0) (.S \> .NP)).any fun r ↦
  r.cat == .S \> .NP && r.subst.apply (.Var 0) == .NP

-- 2段の合成。内側で決まった X = NP が外側の引数にも届く
#guard (fcompGenWithSubst (.S /> .NP) (.Var 0 /> .N /> .Var 0)).any fun r ↦
  r.cat == .S /> .N /> .NP && r.subst.apply (.Var 0) == .NP
#guard (bcompGenWithSubst (.Var 0 \> .N \> .Var 0) (.S \> .NP)).any fun r ↦
  r.cat == .S \> .N \> .NP && r.subst.apply (.Var 0) == .NP

-- スラッシュの向きや引数範疇が合わない場合は拒否
#guard (fcompGenWithSubst (.S /> .NP) (.NP \> .N)).isNone
#guard (bcompGenWithSubst (.NP /> .N) (.S \> .NP)).isNone
#guard (fcompGenWithSubst (.S /> .NP) (.N /> .NP)).isNone
#guard (bcompGenWithSubst (.N \> .NP) (.S \> .NP)).isNone

-- 同じ変数へ矛盾する制約を与える合成は拒否
#guard (fcompGenWithSubst (.S /> (.Var 0 /> .Var 0))
    (.NP /> .S /> .N)).isNone
#guard (bcompGenWithSubst (.NP \> .S \> .N)
    (.S \> (.Var 0 \> .Var 0))).isNone

-- 循環する制約を拒否
#guard (fcompGenWithSubst (.S /> .Var 0) ((.Var 0 \> .NP) /> .N)).isNone
#guard (bcompGenWithSubst ((.Var 0 /> .NP) \> .N) (.S \> .Var 0)).isNone

-- 複数の段で単一化できる場合は、浅い段の最初の成功を採用
#guard (fcompGenWithSubst (.S /> .Var 0) (.NP /> .N /> .NP)).any fun r ↦
  r.cat == (.S /> .NP) && r.subst.apply (.Var 0) == (.NP /> .N)
#guard (bcompGenWithSubst (.NP \> .N \> .NP) (.S \> .Var 0)).any fun r ↦
  r.cat == (.S \> .NP) && r.subst.apply (.Var 0) == (.NP \> .N)

/- # 交差置換の単一化 -/

-- 変数を含まない交差置換。
#guard (fcrossWithSubst (.S /> .NP \> .N) (.NP \> .N)).any fun r ↦
  r.cat == .S \> .N
#guard (bcrossWithSubst (.NP /> .N) (.S \> .NP /> .N)).any fun r ↦
  r.cat == .S /> .N

-- 結果と共有する引数に現れる変数を、同じ置換で具体化する。
#guard (fcrossWithSubst (.Var 0 /> .Var 0 \> .Var 0)
    (.NP \> .NP)).any fun r ↦
  r.cat == .NP \> .NP && r.subst.apply (.Var 0) == .NP
#guard (bcrossWithSubst (.NP /> .NP)
    (.Var 0 \> .Var 0 /> .Var 0)).any fun r ↦
  r.cat == .NP /> .NP && r.subst.apply (.Var 0) == .NP

-- 2つ目の制約で決まった NP を、最初に関連付けた変数にも反映する。
#guard (fcrossWithSubst (.Var 0 /> .Var 0 \> .Var 0)
    (.Var 1 \> .NP)).any fun r ↦
  r.cat == .NP \> .NP &&
    r.subst.apply (.Var 0) == .NP && r.subst.apply (.Var 1) == .NP
#guard (bcrossWithSubst (.Var 1 /> .NP)
    (.Var 0 \> .Var 0 /> .Var 0)).any fun r ↦
  r.cat == .NP /> .NP &&
    r.subst.apply (.Var 0) == .NP && r.subst.apply (.Var 1) == .NP

-- 各制約は単独では解けても、X = NP と X = S を同時には満たせない。
#guard (fcrossWithSubst (.S /> .Var 0 \> .Var 0) (.NP \> .S)).isNone
#guard (bcrossWithSubst (.NP /> .S) (.S \> .Var 0 /> .Var 0)).isNone

-- X0 = X1 と X0 = X1/NP を合わせたときに生じる循環を拒否する。
#guard (fcrossWithSubst (.S /> .Var 0 \> .Var 0)
    (.Var 1 \> (.Var 1 /> .NP))).isNone
#guard (bcrossWithSubst (.Var 1 /> (.Var 1 /> .NP))
    (.S \> .Var 0 /> .Var 0)).isNone

-- Y の不一致と、共有する引数 Z の不一致はそれぞれ拒否する。
#guard (fcrossWithSubst (.S /> .NP \> .N) (.S \> .N)).isNone
#guard (bcrossWithSubst (.S /> .N) (.S \> .NP /> .N)).isNone
#guard (fcrossWithSubst (.S /> .NP \> .NP) (.NP \> .N)).isNone
#guard (bcrossWithSubst (.NP /> .N) (.S \> .NP /> .NP)).isNone

-- 内側または外側のスラッシュの向きが規則に合わない場合は拒否する。
#guard (fcrossWithSubst (.S \> .NP \> .N) (.NP \> .N)).isNone
#guard (bcrossWithSubst (.NP /> .N) (.S /> .NP /> .N)).isNone
#guard (fcrossWithSubst (.S /> .NP /> .N) (.NP \> .N)).isNone
#guard (bcrossWithSubst (.NP \> .N) (.S \> .NP /> .N)).isNone
