type ty = 
  | Base of string
  | Implication of ty*ty
  | False

type gam = (string * ty) list