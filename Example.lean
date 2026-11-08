import LeanCcg

def exampleLexicon : Lexicon := {
  ("Keats",  [.NP]),
  ("eats",   [.S \> .NP /> .NP]),
  ("apples", [.NP])
}

-- 文全体の範疇が S となる導出木を表示する。
#eval (parse exampleLexicon "Keats eats apples").filter
  (fun t => t.cat == .S)

#guard (parse exampleLexicon "Keats eats apples").any
  (fun t => t.cat == .S)
