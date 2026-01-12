/- # Utilities -/

abbrev Token := String -- 語(token)の型

def String.replicateStr (str : String) (n : Nat) : String :=
  String.join <| List.replicate n str

def String.toTokens (str : String) : List Token :=
  str.splitOn.filter (· ≠ "")
