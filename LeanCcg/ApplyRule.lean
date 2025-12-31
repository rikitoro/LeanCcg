import LeanCcg.Cat

/-- 順関数適用 (>) x/y y ⟹ x -/
def fapp : Cat → Cat → Option Cat
  | .Fun .Fwd x y, y' => if y == y' then some x else none
  | _, _  => none

/-- 逆関数適用 (<) y x\y ⟹ x -/
def bapp : Cat → Cat → Option Cat
  | y', .Fun .Bwd x y => if y == y' then some x else none
  | _, _ => none

/-- 順関数合成 (> B) x/y y/z ⟹ x/z -/
def fcomp : Cat → Cat → Option Cat
  | .Fun .Fwd x y, .Fun .Fwd y' z =>
    if y == y' then some (x /> z) else none
  | _, _ => none

/-- 逆関数合成 (< B) y\z x\y ⟹ x\z -/
def bcomp : Cat → Cat → Option Cat
  | .Fun .Bwd y' z, .Fun .Bwd x y => if y == y' then some (x \> z) else none
  | _, _ => none

/-- 一般化順関数合成 (> Bn) x/y y/$/w ⟹ x/$/w -/
def fcompGen : Cat → Cat → Option Cat
  | .Fun .Fwd x y, .Fun .Fwd y' z =>
    if y == y' then
      some (x /> z)
    else
      match fcompGen (x /> y) y' with
      | some x' => some (x' /> z)
      | none => none
  | _, _ => none

/-- 一般化逆関数合成 (< Bn) y\$\z x\y ⟹ x\$\z -/
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
#eval fcomp (.S /> .NP) (.NP /> .NP) -- some S/NP
#eval bapp .NP (.S \> .NP) -- some S
#eval bcomp (.NP \> .NP) (.S \> .NP) -- some S\NP
#eval fcompGen (.S /> .NP) (.NP /> .NP) -- some S/NP
#eval bcompGen (.S \> .NP \> .NP) (.S \> .S) --some S\NP\NP
