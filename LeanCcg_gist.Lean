/- ## Utilities -/
abbrev Token := String

def String.replicateStr (str : String) (n : Nat) : String :=
  String.join <| List.replicate n str

def String.toTokens (str : String) : List Token :=
  str.splitOn.filter (· ≠ "")

/- ## 範疇 Cat の定義 -/

/-- 前方適用 '/' 後方適用 '\' -/
inductive Dir
  | Fwd -- /
  | Bwd -- \
  deriving BEq

def Dir.toString : Dir → String
  | .Fwd => "/"
  | .Bwd => "\\"

instance : ToString Dir where
  toString := Dir.toString

/-- 範疇 Cat -/
inductive Cat : Type
  | S   : Cat
  | NP  : Cat
  | N   : Cat
  | Fun : Dir → Cat → Cat → Cat -- x / y or x \ y
  deriving BEq

infixl:70 " /> "   => Cat.Fun Dir.Fwd
infixl:70 " \\> "  => Cat.Fun Dir.Bwd

def Cat.toString : Cat → String
  | .S  => "S"
  | .NP => "NP"
  | .N  => "N"
  | .Fun d x y =>
    match x, y with
    | .Fun .., .Fun .. => "(" ++ x.toString ++ ")" ++ d.toString ++ "(" ++ y.toString ++ ")"
    | .Fun .., _       => "(" ++ x.toString ++ ")" ++ d.toString ++ y.toString
    | _      , .Fun .. => x.toString ++ d.toString ++ "(" ++ y.toString ++ ")"
    | _      , _       => x.toString ++ d.toString ++ y.toString

instance : ToString Cat where
  toString := Cat.toString

/- ## 組合せ規則の定義 -/


inductive Rule : Type
  | Fa  -- forward app [>]
  | Ba  -- backward app [<]
  | Fc  -- forward comp gen [>B]
  | Bc  -- backward comp gen [<B]
  | Ft  -- forward type raising [>T]
  | Bt  -- backward type raising [<T]

def Rule.toString : Rule → String
  | .Fa => "[>]"
  | .Ba => "[<]"
  | .Fc => "[>B]"
  | .Bc => "[<B]"
  | .Ft => "[>T]"
  | .Bt => "[<T]"

instance : ToString Rule where
  toString := Rule.toString

/- ## 導出木 Tree の定義 -/

inductive Tree : Type
  | leaf (tok : Token) (c : Cat)
  | branch (r : Rule) (c : Cat) (lt rt : Tree)
  | unary (r : Rule) (c : Cat) (t : Tree) -- type raing 用

private def Tree.toStringAux (n : Nat) : Tree → String
  | .leaf t c =>
    pre n ++
    c.toString ++ " '" ++ t ++ "'"
  | .branch r c lt rt =>
    pre n ++ s!"{c} {r}\n" ++
    -- c.toString ++ " [" ++ r.toString ++ "]\n" ++
    toStringAux (n + 1) lt ++ "\n" ++
    toStringAux (n + 1) rt
  | .unary r c t =>
    pre n ++ s!"{c} {r}\n" ++
    toStringAux (n + 1) t
  where
    pre n := "| ".replicateStr n

def Tree.toString (t : Tree) : String :=
  "\n" ++ t.toStringAux 0

instance : ToString Tree where
  toString := Tree.toString

def Tree.cat : Tree → Cat
  | .leaf _ c => c
  | .branch _ c _ _ => c
  | .unary _ c _ => c

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

/- ### 導出木への組合せ規則の適用 -/

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


/- ## 型繰り上げ規則 (Type raising) の適用 -/

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

/- ### 導出木への型繰り上げ規則の適用 -/

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


/- ## CYKアルゴリズムによるチャートパーシング -/

/- ### 各セルの定義 -/
structure Cell where
  span  : Nat
  index : Nat
  trees : List Tree

def Cell.toString : Cell → String
  | ⟨span, index, trees⟩ => s!"⟨{span}, {index}, {trees}⟩"

instance : ToString Cell where
  toString := Cell.toString

abbrev Chart := List Cell

def Chart.lookup (chart : Chart) (span index : Nat) : List Tree :=
  let cell : Option Cell := chart.find? <| fun ⟨s', i', _⟩ ↦ s' == span && i' = index
  match cell with
  | some ⟨_, _, ts⟩ => ts
  | none => []

/-- ### CYK パーシング -/
def fillChart (toks : List Token) (lexicon : Token → List Cat) : Chart := Id.run do
  let len := toks.length
  -- リーフノードを作成 (span = 1)
  let mut chart : Chart :=
    toks.zipIdx.map <| fun (t, i) ↦
      let leaves : List Tree := (lexicon t).map (.leaf t ·)
      let leaves' : List Tree := leaves.flatMap (raiseTree ·) -- Type raising したもの
      ⟨1, i, leaves ++ leaves'⟩
  -- ボトムアップに導出木を結合　(span ≥ 2)
  for span in 2...(len + 1) do
    for i in 0...(len - span + 1) do
      let mut trees : List Tree := []
      for k in 1...span do
        let lts := chart.lookup k i
        let rts := chart.lookup (span - k) (i + k)
        trees := trees ++
          lts.flatMap fun lt ↦ rts.flatMap fun rt ↦ combineTree lt rt
      let trees' : List Tree := trees.flatMap (raiseTree ·) -- Type raising したもの
      chart := chart.concat ⟨span, i, trees ++ trees'⟩
  return chart

/- ## 語彙辞書 lexicon の定義 -/

def lexicon1 : Token → List Cat
  | "Keats"   => [.NP]
  | "Sid"     => [.NP]
  | "eats"    => [.S \> .NP /> .NP]
  | "cooks"   => [.S \> .NP /> .NP]
  | "an"      => [.NP /> .N]
  | "apple"   => [.N]
  | "apples"  => [.NP]
  | _         => []

def lexicon2 : Token → List Cat
  | "I"       => [.S /> (.S \> .NP)]
  | "think"   => [.S \> .NP /> .S,]
  | "that"    => [.S /> .S, .NP /> .N, .N, .N \> .N /> (.S /> .NP)]
  | "boy"     => [.N]
  | "wrote"   => [.S \> .NP /> .NP]
  | "is"      => [.S \> .NP /> (.S \> .NP)]
  | "wrong"   => [.S \> .NP]
  | _ => []

/- ## parser の定義 -/
def parseCCG  (lexicon : Token → List Cat) (toks : List Token) : List Tree :=
  let chart := fillChart toks lexicon
  let len := toks.length
  chart.lookup len 0

def parse  (lexicon : Token → List Cat) (sentence : String): List Tree :=
  parseCCG lexicon sentence.toTokens


-- ## #eval で parse を評価して試せます

#eval parse lexicon1 "Keats eats apples"
-- [
-- S [<]
-- | NP 'Keats'
-- | S\NP [>]
-- | | (S\NP)/NP 'eats'
-- | | NP 'apples',
-- S [>]
-- | S/(S\NP) [>T]
-- | | NP 'Keats'
-- | S\NP [>]
-- | | (S\NP)/NP 'eats'
-- | | NP 'apples',
-- S [>]
-- | S/NP [>B]
-- | | S/(S\NP) [>T]
-- | | | NP 'Keats'
-- | | (S\NP)/NP 'eats'
-- | NP 'apples',
-- S [<]
-- | S/NP [>B]
-- | | S/(S\NP) [>T]
-- | | | NP 'Keats'
-- | | (S\NP)/NP 'eats'
-- | S\(S/NP) [<T]
-- | | NP 'apples']

#eval parse lexicon1 "Keats eats an apple"

#eval parse lexicon2 "I think that that that that that boy wrote is wrong"
