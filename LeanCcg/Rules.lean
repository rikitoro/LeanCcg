import LeanCcg.Syntax
import LeanCcg.Rule
import LeanCcg.Derivation

/- ## 組合せ規則の適用 -/

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

/- ## Type raising -/

/-- Forward Type rising
  [>T] x ⟹ t/(t\x) : 今回は x = NP, t = S に限定する -/
def ftraise : Cat → Option Cat
  | .NP => some (.S /> (.S \> .NP))
  | _ => none

/-- Backward Type rising
  [<T] x ⟹ t\ (t/x) : 今回は x = NP, t = S に限定する -/
def btraise : Cat → Option Cat
  | .NP => some (.S \> (.S /> .NP))
  | _ => none



def Rule.applyBinary : Rule → Cat → Cat → Option Cat
  | .Fa, lc, rc => fapp lc rc
  | .Ba, lc, rc => bapp lc rc
  | .Fc, lc, rc => fcompGen lc rc
  | .Bc, lc, rc => bcompGen lc rc
  | _, _, _ => none

def Rule.applyUnary : Rule → Cat → Option Cat
  | .Ft, c => ftraise c
  | .Bt, c => btraise c
  | _, _ => none




def tryBinaryRules (lc rc : Cat) : List (Rule × Cat) :=
  let binaryRules : List Rule := [.Fa, .Ba, .Fc, .Bc]
  binaryRules.filterMap fun r ↦
    (r.applyBinary lc rc).map (r, ·)

def tryUnaryRules (c : Cat) : List (Rule × Cat) :=
  let unaryRules  : List Rule := [.Ft, .Bt]
  unaryRules.filterMap fun r ↦
    (r.applyUnary c).map (r, ·)


def combineTree (lt rt : Tree) : List Tree :=
  let applied := tryBinaryRules lt.cat rt.cat
  applied.map fun (r, c) ↦ .branch r c lt rt

def raiseTree (t : Tree) : List Tree :=
  let applied := tryUnaryRules t.cat
  applied.map fun (r, c) ↦ .unary r c t
