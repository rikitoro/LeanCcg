abbrev Token := String

def String.replicate (str : String) : Nat → String
  | 0 => ""
  | n + 1 => str ++ replicate str n

#eval "| ".replicate 3
