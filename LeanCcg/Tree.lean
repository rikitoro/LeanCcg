import LeanCcg.Util
import LeanCcg.Cat

/- ## 適用ルール -/

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
    s!"{c} \"{t}\""
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

#eval Tree.leaf "John" .NP
#eval Tree.leaf "sleeps" (.S \> .NP)

#eval
  Tree.branch .Ba .S
    (.branch .Ba .NP
      (.leaf "the" (.NP /> .N))
      (.leaf "dog" .N))
    (.leaf "Sleeps" (.S \> .NP))
#eval
  Tree.unary .Bt (.S \> (.S /> .NP))
    (.leaf "Keats" .NP)

def Tree.cat : Tree → Cat
  | .leaf _ c => c
  | .branch _ c _ _ => c
  | .unary _ c _ => c
