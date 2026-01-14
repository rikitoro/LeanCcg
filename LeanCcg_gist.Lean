import Std.Data.HashMap

section Utility
/- # Utilities -/

abbrev Token := String -- 語(token)の型

def String.replicateStr (str : String) (n : Nat) : String :=
  String.join <| List.replicate n str

def String.toTokens (str : String) : List Token :=
  str.splitOn.filter (· ≠ "")

end Utility

section Category
/- # 統語範疇 (Cat) の定義 -/

/-- 関数適用の向き -/
inductive Dir : Type
  | Fwd -- /
  | Bwd -- \
  deriving BEq

/-- 範疇 Cat -/
inductive Cat : Type
  | S   : Cat
  | NP  : Cat
  | N   : Cat   -- 基底範疇 (S, NP, N)
  | Fun : Dir → Cat → Cat → Cat -- 関数型範疇 x/y or x\y
  deriving BEq

/-- 関数型範疇の記法 -/
infixl:70 " /> "   => Cat.Fun Dir.Fwd -- x/y を x /> y で表記
infixl:70 " \\> "  => Cat.Fun Dir.Bwd -- x\y を x \> y で表記

---

def Dir.toString : Dir → String
  | .Fwd => "/"
  | .Bwd => "\\"

instance : ToString Dir where
  toString := Dir.toString

def Cat.toString : Cat → String
  | .S  => "S"  | .NP => "NP"  | .N  => "N"
  | .Fun d x y =>
    match x, y with
    | .Fun .., .Fun .. => "(" ++ x.toString ++ ")" ++ d.toString ++ "(" ++ y.toString ++ ")"
    | .Fun .., _       => "(" ++ x.toString ++ ")" ++ d.toString ++ y.toString
    | _      , .Fun .. => x.toString ++ d.toString ++ "(" ++ y.toString ++ ")"
    | _      , _       => x.toString ++ d.toString ++ y.toString

instance : ToString Cat where
  toString := Cat.toString

end Category

section Rules
/- # CCGの組合せ規則 (BinaryRule, UnaryRule) -/

/- ## 組合せ規則の一覧 -/

inductive BinaryRule : Type
  | Fa  -- forward app [>]
  | Ba  -- backward app [<]
  | Fc  -- forward comp gen [>B]
  | Bc  -- backward comp gen [<B]

inductive UnaryRule : Type
  | Ft  -- forward type raising [>T]
  | Bt  -- backward type raising [<T]

def binaryRules : List BinaryRule := [.Fa, .Ba, .Fc, .Bc]
def unaryRules  : List UnaryRule  := [.Ft, .Bt]

---

def BinaryRule.toString : BinaryRule → String
  | .Fa => "[>]"
  | .Ba => "[<]"
  | .Fc => "[>B]"
  | .Bc => "[<B]"

def UnaryRule.toString : UnaryRule → String
  | .Ft => "[>T]"
  | .Bt => "[<T]"

instance : ToString BinaryRule where
  toString := BinaryRule.toString

instance : ToString UnaryRule where
  toString := UnaryRule.toString


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


/- ## Rule との対応付け -/

def BinaryRule.applyBinary : BinaryRule → Cat → Cat → Option Cat
  | .Fa => fapp
  | .Ba => bapp
  | .Fc => fcompGen
  | .Bc => bcompGen

def UnaryRule.applyUnary : UnaryRule → Cat → Option Cat
  | .Ft => fraise
  | .Bt => braise

end Rules

section Tree
/- # 導出木 (Tree) の定義 -/

inductive Tree : Type
  | leaf (tok : Token) (c : Cat)
  | branch (r : BinaryRule) (c : Cat) (lt rt : Tree)
  | unary (r : UnaryRule) (c : Cat) (t : Tree)


def Tree.cat : Tree → Cat
  | .leaf _ c => c
  | .branch _ c _ _ => c
  | .unary _ c _ => c

---

private def Tree.toStringAux (n : Nat) : Tree → String
  | .leaf t c =>
    pre n ++
    s!"{c} \"{t}\""
  | .branch r c lt rt =>
    pre n ++ s!"{c} {r}\n" ++
    toStringAux (n + 1) lt ++ "\n" ++
    toStringAux (n + 1) rt
  | .unary r c t =>
    pre n ++ s!"{c} {r}\n" ++
    toStringAux (n + 1) t
  where
    pre : Nat → String
    | 0 => ""
    | n + 1 => "│".replicateStr n ++ "├"

def Tree.toString (t : Tree) : String :=
  "\n" ++ t.toStringAux 0

instance : ToString Tree where
  toString := Tree.toString

end Tree

section Applying
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

end Applying

section Parsing
/- # CYKアルゴリズムによるチャートパーシング -/

/-- 範疇辞書の型 -/
abbrev Lexicon := Std.HashMap Token (List Cat)

def Lexicon.getCats (lex : Lexicon) (t : Token) : List Cat :=
  lex.getD t []


/-- chart の型
  key : (span, i), value : index = i ~ i+span の部分の導出木の集まり
-/
abbrev Chart := Std.HashMap (Nat × Nat) (List Tree) -- key : (span, i)

def Chart.getTrees (chart : Chart) (span_i : Nat×Nat) : List Tree :=
  chart.getD span_i []

/-- ## CYK パーシング -/
def fillChart (lex : Lexicon) (toks : List Token)  : Chart := Id.run do
  let len := toks.length
  let mut chart : Chart := {}
  -- リーフノードを作成 (span = 1)
  let span : Nat := 1
  for (t, i) in toks.zipIdx do
    let leaves : List Tree := (lex.getCats t).map (.leaf t ·)
    let raised : List Tree := leaves.flatMap (raiseTree ·)
    chart := chart.insert (span, i) (leaves ++ raised)
  -- ボトムアップに導出木を結合 (span ≥ 2)
  for span in 2...(len + 1) do
    for i in 0...(len - span + 1) do
      let mut trees : List Tree := []
      for k in 1...span do
        let lts := chart.getTrees (k, i)
        let rts := chart.getTrees (span - k, i + k)
        trees := trees ++
          lts.flatMap fun lt ↦ rts.flatMap fun rt ↦ combineTree lt rt
      let raised : List Tree := trees.flatMap (raiseTree ·) -- Type raising したもの
      chart := chart.insert (span, i) (trees ++ raised)
  return chart

def parseCCG  (lex : Lexicon) (toks : List Token) : List Tree :=
  let chart := fillChart lex toks
  let len := toks.length
  chart.getTrees (len, 0)

def parse  (lex : Lexicon) (sentence : String): List Tree :=
  let toks : List Token := sentence.toTokens
  parseCCG lex toks

end Parsing

section Example
/- # Examples -/

def lexicon1 : Lexicon := {
  ("Keats",   [.NP]),
  ("eats",    [.S \> .NP /> .NP]),
  ("cooks",   [.S \> .NP /> .NP]),
  ("an",      [.NP /> .N]),
  ("apple",   [.N]),
  ("apples",  [.NP])}

#eval parse lexicon1 "Keats eats apples"
-- [
-- S [<]
-- ├NP "Keats"
-- ├S\NP [>]
-- │├(S\NP)/NP "eats"
-- │├NP "apples",
-- S [>]
-- ├S/(S\NP) [>T]
-- │├NP "Keats"
-- ├S\NP [>]
-- │├(S\NP)/NP "eats"
-- │├NP "apples",
-- S [>]
-- ├S/NP [>B]
-- │├S/(S\NP) [>T]
-- ││├NP "Keats"
-- │├(S\NP)/NP "eats"
-- ├NP "apples",
-- S [<]
-- ├S/NP [>B]
-- │├S/(S\NP) [>T]
-- ││├NP "Keats"
-- │├(S\NP)/NP "eats"
-- ├S\(S/NP) [<T]
-- │├NP "apples"]

#eval parse lexicon1 "Keats eats an apple"

def lexicon2 : Lexicon :={
  ("I",        [.S /> (.S \> .NP)]),
  ("think",    [.S \> .NP /> .S,]),
  ("that",    [.S /> .S, .NP /> .N, .N, .N \> .N /> (.S /> .NP)]),
  ("boy",     [.N]),
  ("wrote",   [.S \> .NP /> .NP]),
  ("is",      [.S \> .NP /> (.S \> .NP)]),
  ("wrong",   [.S \> .NP])}

#eval parse lexicon2 "I think that that that that that boy wrote is wrong"

end Example
