(* Type of a term / goal of a proof *)
type ty =
  | Base of string (* A base type X *)
  | Implication of ty * ty (* A -> B *)
  | False
  | True
  | Conjunction of ty * ty (* A /\ B *)
  | Disjunction of ty * ty (* A \/ B *)

(* Typing environment for type inference (variable name, type) *)
type gam = (string * ty) list
