type ltype = 
  | Base of string
  | Implication of ltype*ltype
  | False