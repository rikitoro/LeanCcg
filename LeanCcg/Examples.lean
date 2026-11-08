import LeanCcg.Parsing

/- # Examples -/

def lexicon1 : Lexicon := {
  ("Keats",    [.NP]),
  ("eats",     [.S \> .NP /> .NP]),
  ("cooks",    [.S \> .NP /> .NP]),
  ("an",       [.NP /> .N]),
  ("apple",    [.N]),
  ("apples",   [.NP])}

#eval parse lexicon1 "Keats eats apples"
#eval parse lexicon1 "Keats eats an apple"

def lexicon2 : Lexicon := {
  ("I",       [.S /> (.S \> .NP)]),
  ("think",   [.S \> .NP /> .S,]),
  ("that",    [.S /> .S, .NP /> .N, .N, .N \> .N /> (.S /> .NP)]),
  ("boy",     [.N]),
  ("wrote",   [.S \> .NP /> .NP]),
  ("is",      [.S \> .NP /> (.S \> .NP)]),
  ("wrong",   [.S \> .NP])}

#eval parse lexicon2 "I think that that that that that boy wrote is wrong"

def lexicon3 : Lexicon := {
  ("Keats",     [.NP]),
  ("will",      [.S \> .NP /> (.S \> .NP)]),
  ("file",      [.S \> .NP /> .NP]),
  ("without",   [.S \> .NP \> (.S \> .NP) /> (.S \> .NP)]),
  ("reading",   [.S \> .NP /> .NP]),
  ("these",     [.NP /> .NP]),
  ("articles",  [.NP])}

#eval parse lexicon3 "file without reading"
#eval parse lexicon3 "Keats will file without reading these articles"


/- # 範疇変数を持つ and による等位接続 -/

private def coordinationCat : Cat :=
  let x := Cat.Var 0
  x \> x /> x

def lexcon4 : Lexicon := {
  ("and",     [coordinationCat]),
  ("apples",  [.NP]),
  ("pears",   [.NP]),
  ("runs",    [.S \> .NP]),
  ("sleeps",  [.S \> .NP])
}

#eval (parse lexcon4 "and apples").filter (fun t ↦ t.cat == (.NP \> .NP))
#eval (parse lexcon4 "apples and pears").filter (fun t ↦ t.cat == .NP)
#eval (parse lexcon4 "runs and sleeps").filter (fun t ↦ t.cat == .S \> .NP)

-- and の葉に置換が反映される
#guard (parse lexcon4 "and apples").any fun
  | .branch .Fa c (.leaf _ lc) (.leaf _ rc) =>
    c == (.NP \> .NP) && lc == .NP \> .NP /> .NP && rc == .NP
  | _ => false

#guard (parse lexcon4 "apples and pears").any fun t ↦ t.cat == .NP

-- and を動詞句に使う
#guard
  let vp : Cat := .S \> .NP
  (parse lexcon4 "runs and sleeps").any fun
    | .branch .Ba c (.leaf _ lc) (.branch .Fa rc (.leaf _ andCat) (.leaf _ sc)) =>
      c == vp && lc == vp && rc == vp \> vp && andCat == vp \> vp /> vp && sc == vp
    | _ => false


/- # 変数スコープと置換の伝播の確認 -/

private def scopeLexicon : Lexicon := {
  ("id",      [.Var 0 /> .Var 0]),
  ("discard", [.NP /> .Var 0]),
  ("slot",    [.Var 1]),
  ("apples",  [.NP]),
  ("same",    [.S /> (.Var 0 /> .Var 0)]),
  ("bad",     [.NP /> .S])
}

-- 同じ語を2回使うときはそれぞれの変数を独立に扱う

#eval (parse scopeLexicon "id id")
