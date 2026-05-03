type ty = 
  | Base of string
  | Implication of ty*ty
  | False
  | True
  | Conjunction of ty*ty
  | Disjunction of ty*ty

type gam = (string * ty) list