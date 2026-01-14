import LeanCcg.Applying
import Std

/- # CYKアルゴリズムによるチャートパーシング -/

/-- 範疇辞書の型 -/
abbrev Lexicon := Std.HashMap Token (List Cat)

def Lexicon.getCats (lex : Lexicon) (t : Token) : List Cat :=
  lex.getD t []


/-- chart の型
  key : (span, i), value : index = i ~ i+span の部分の導出木の集まり
-/
abbrev Chart := Std.HashMap (Nat × Nat) (List Tree) -- key : (span, i)

def Chart.getTrees (chart : Chart) (span_i : Nat×Nat) :=
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
