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
