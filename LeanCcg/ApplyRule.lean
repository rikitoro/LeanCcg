import LeanCcg.Cat
import LeanCcg.Tree

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

#eval fapp (.S /> .NP) .NP -- some S
#eval bapp .NP (.S \> .NP) -- some S
#eval bcompGen (.NP \> .NP) (.S \> .NP) -- some S\NP
#eval fcompGen (.S /> .NP) (.NP /> .NP) -- some S/NP
#eval bcompGen (.S \> .NP \> .NP) (.S \> .S) --some S\NP\NP


def tryBinaryRules (lc rc : Cat) : List (Rule × Cat) :=
  let rules : List (Rule × Option Cat) := [
    (.Fa, fapp lc rc),
    (.Ba, bapp lc rc),
    (.Fc, fcompGen lc rc),
    (.Bc, bcompGen lc rc)
  ]
  rules.filterMap fun (rule, res) ↦
    res.map fun c ↦ (rule, c)

def combineTree (lt rt : Tree) : List Tree :=
  let applied := tryBinaryRules lt.cat rt.cat
  applied.map fun (r, c) ↦ .branch r c lt rt

#eval combineTree
  (Tree.branch .Ba .NP
    (.leaf "the" (.NP /> .N))
    (.leaf "dog" .N))
  (.leaf "Sleeps" (.S \> .NP))
#eval combineTree
  (.leaf "might" (.S \> .NP /> (.S \> .NP)))
  (.leaf "eat" (.S \> .NP /> .NP))

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

#eval ftraise .NP
#eval ftraise .N
#eval btraise .NP

def tryUnaryRules (c : Cat) : List (Rule × Cat) :=
  let rules : List (Rule × Option Cat) := [
    (.Ft, ftraise c),
    (.Bt, btraise c)
  ]
  rules.filterMap fun (rule, res) ↦
    res.map fun c ↦ (rule, c)

def raiseTree (t : Tree) : List Tree :=
  let applied := tryUnaryRules t.cat
  applied.map fun (r, c) ↦ .unary r c t
