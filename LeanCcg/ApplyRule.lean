import LeanCcg.Cat
import LeanCcg.Tree

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

/- Forward functional composition
  [>B] x/y y/z ⟹ x/z -/
-- def fcomp : Cat → Cat → Option Cat
--   | .Fun .Fwd x y, .Fun .Fwd y' z =>
--     if y == y' then some (x /> z) else none
--   | _, _ => none

/- Backward functional composition
  [<B] y\z x\y ⟹ x\z -/
-- def bcomp : Cat → Cat → Option Cat
--   | .Fun .Bwd y' z, .Fun .Bwd x y =>
--     if y == y' then some (x \> z) else none
--   | _, _ => none

/-- Generalized forward functional composition
  [>Bn] x/y y/$/w ⟹ x/$/w -/
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
  [<Bn] y\$\z x\y ⟹ x\$\z -/
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
--#eval fcomp (.S /> .NP) (.NP /> .NP) -- some S/NP
#eval bapp .NP (.S \> .NP) -- some S
#eval bcompGen (.NP \> .NP) (.S \> .NP) -- some S\NP
#eval fcompGen (.S /> .NP) (.NP /> .NP) -- some S/NP
#eval bcompGen (.S \> .NP \> .NP) (.S \> .S) --some S\NP\NP


def tryRules (lt rt : Tree) : List Tree :=
  let rules : List (Rule × Option Cat) := [
    (.Fa,  fapp lt.cat rt.cat),
    (.Ba,  bapp lt.cat rt.cat),
    (.Fcg, fcompGen lt.cat rt.cat),
    (.Bcg, bcompGen lt.cat rt.cat)
  ]
  rules.filterMap <|
    fun (rule, res) ↦ res.map <| fun c ↦ .branch rule c lt rt

#eval tryRules
  (Tree.branch .Ba .NP
    (.leaf "the" (.NP /> .N))
    (.leaf "dog" .N))
  (.leaf "Sleeps" (.S \> .NP))
