/- # 統語範疇 (Cat) の定義 -/

/-- 関数適用の向き -/
inductive Dir : Type
  | Fwd -- /
  | Bwd -- \
  deriving BEq

abbrev VarId := Nat

/-- 範疇 Cat -/
inductive Cat : Type
  | S   : Cat
  | NP  : Cat
  | N   : Cat   -- 基底範疇 (S, NP, N)
  | Var : VarId → Cat
  | Fun : Dir → Cat → Cat → Cat -- 関数型範疇 x/y or x\y
  deriving BEq

/-- 範疇の中に変数 v が表れるか -/
def Cat.occurs (v : VarId) : Cat → Bool
  | .Var w      => v == w
  | .Fun _ x y  => Cat.occurs v x || Cat.occurs v y
  | _           => false


/-- 関数型範疇の記法 -/
infixl:70 " /> "   => Cat.Fun Dir.Fwd -- x/y を x /> y で表記
infixl:70 " \\> "  => Cat.Fun Dir.Bwd -- x\y を x \> y で表記




def Dir.toString : Dir → String
  | .Fwd => "/"
  | .Bwd => "\\"

instance : ToString Dir where
  toString := Dir.toString

def Cat.toString : Cat → String
  | .S  => "S"  | .NP => "NP"  | .N  => "N"
  | .Var v => s!"X{v}"
  | .Fun d x y =>
    match x, y with
    | .Fun .., .Fun .. => s!"{x.toString}{d}({y.toString})" -- x.toString ++ d.toString ++ "(" ++ y.toString ++ ")"
    | .Fun .., _       => s!"{x.toString}{d}{y.toString}" -- x.toString ++ d.toString ++ y.toString
    | _      , .Fun .. => s!"{x.toString}{d}({y.toString})" -- x.toString ++ d.toString ++ "(" ++ y.toString ++ ")"
    | _      , _       => s!"{x.toString}{d}{y.toString}" -- x.toString ++ d.toString ++ y.toString

instance : ToString Cat where
  toString := Cat.toString


#guard Cat.occurs 0 (.S /> .Var 0)
