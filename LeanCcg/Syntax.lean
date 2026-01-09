import LeanCcg.Util

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
