/- ## 適用ルール -/

inductive BinaryRule : Type
  | Fa  -- forward app [>]
  | Ba  -- backward app [<]
  | Fc  -- forward comp gen [>B]
  | Bc  -- backward comp gen [<B]
  | Fx  -- forward crossed subst [>Sx]
  | Bx  -- backward crossed subst [<Sx]

inductive UnaryRule : Type
  | Ft  -- forward type raising [>T]
  | Bt  -- backward type raising [<T]

def binaryRules : List BinaryRule := [.Fa, .Ba, .Fc, .Bc, .Fx, .Bx]
def unaryRules  : List UnaryRule  := [.Ft, .Bt]

def BinaryRule.toString : BinaryRule → String
  | .Fa => "[>]"
  | .Ba => "[<]"
  | .Fc => "[>B]"
  | .Bc => "[<B]"
  | .Fx => "[>Sx]"
  | .Bx => "[<Sx]"

def UnaryRule.toString : UnaryRule → String
  | .Ft => "[>T]"
  | .Bt => "[<T]"

instance : ToString BinaryRule where
  toString := BinaryRule.toString

instance : ToString UnaryRule where
  toString := UnaryRule.toString
