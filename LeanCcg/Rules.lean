import LeanCcg.Category

/- # CCGの組合せ規則 (BinaryRule, UnaryRule) -/

/- ## 組合せ規則の適用 -/

-- ### Binary Rules

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

def UnaryRule.applyUnary : UnaryRule → Cat → Option Cat
  | .Ft => fraise
  | .Bt => braise
