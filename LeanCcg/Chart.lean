import LeanCcg.ApplyRule

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
      let leafs : List Tree := (lexicon t).map <| fun c ↦ .leaf t c
      ⟨1, i, leafs⟩
  -- ボトムアップに導出木を結合　(span ≥ 2)
  for span in 2...(len + 1) do
    for i in 0...(len - span + 1) do
      let mut trees : List Tree := []
      for k in 1...span do
        let lts := chart.lookup k i
        let rts := chart.lookup (span - k) (i + k)
        trees := trees ++
          lts.flatMap fun lt ↦ rts.flatMap fun rt ↦ combineTree lt rt
      chart := chart.concat ⟨span, i, trees⟩
  return chart

-- #eval fillChart ["Keats"]
-- #eval fillChart ["Keats", "eats"]
-- #eval fillChart ["Keats", "eats", "an"]
