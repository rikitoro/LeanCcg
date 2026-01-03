abbrev Token := String

def String.replicateStr (str : String) (n : Nat) : String :=
  String.join <| List.replicate n str

#eval "| ".replicateStr 3
#eval "| ".replicateStr 0

def String.toTokens (str : String) : List Token :=
  str.splitOn.filter (· ≠ "")

#eval "John  likes a dog ".toTokens
