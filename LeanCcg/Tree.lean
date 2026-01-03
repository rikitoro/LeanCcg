import LeanCcg.Util
import LeanCcg.Cat

/- ## 適用ルール -/

inductive Rule : Type
  | Fa -- forward app [>]
  | Ba -- backward app [<]
  | Fcg -- forward comp gen [>B]
  | Bcg -- backward comp gen [<B]

def Rule.toString : Rule → String
  | .Fa => ">"
  | .Ba => "<"
  | .Fcg => ">B"
  | .Bcg => "<B"

instance : ToString Rule where
  toString := Rule.toString


/- ## 導出木 Tree の定義 -/

inductive Tree : Type
  | leaf (tok : String) (c : Cat)
  | branch (r : Rule) (c : Cat) (lt rt : Tree)


private def Tree.toStringAux (n : Nat) : Tree → String
  | .leaf t c =>
    pre n ++
    c.toString ++ " '" ++ t ++ "'"
  | .branch r c lt rt =>
    pre n ++
    c.toString ++ " [" ++ r.toString ++ "]\n" ++
    toStringAux (n + 1) lt ++ "\n" ++
    toStringAux (n + 1) rt
  where
    pre n := "| ".replicateStr n

def Tree.toString (t : Tree) : String :=
  "\n" ++ t.toStringAux 0

instance : ToString Tree where
  toString := Tree.toString

#eval Tree.leaf "John" .NP
#eval Tree.leaf "sleeps" (.S \> .NP)

#eval
  Tree.branch .Ba .S
    (.branch .Ba .NP
      (.leaf "the" (.NP /> .N))
      (.leaf "dog" .N))
    (.leaf "Sleeps" (.S \> .NP))

def Tree.cat : Tree → Cat
  | .leaf _ c => c
  | .branch _ c _ _ => c
