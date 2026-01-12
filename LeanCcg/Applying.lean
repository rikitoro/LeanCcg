import LeanCcg.Rules
import LeanCcg.Tree

/- ## 適用規則の Tree への適用 -/

def tryBinaryRules (lc rc : Cat) : List (BinaryRule × Cat) :=
  binaryRules.filterMap fun r ↦
    (r.applyBinary lc rc).map (r, ·)

def tryUnaryRules (c : Cat) : List (UnaryRule × Cat) :=
  unaryRules.filterMap fun r ↦
    (r.applyUnary c).map (r, ·)

def combineTree (lt rt : Tree) : List Tree :=
  let applied := tryBinaryRules lt.cat rt.cat
  applied.map fun (r, c) ↦ .branch r c lt rt

def raiseTree (t : Tree) : List Tree :=
  let applied := tryUnaryRules t.cat
  applied.map fun (r, c) ↦ .unary r c t
