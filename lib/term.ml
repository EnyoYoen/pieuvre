open Type

type lam =
  | Abstraction of string*ltype*lam
  | Application of lam*lam
  | Variable of string
  | ExFalso of lam*ltype

let alpha (_ : lam) (_ : lam) = 
  false

let betastep (l : lam) = 
  Some (l)

let reduce (_ : lam) = 
  ()