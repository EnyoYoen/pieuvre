open Exceptions
open Term
open Type

type proof =
  | Abstraction of string*ty*proof
  | Application of proof*proof
  | Variable of string
  | ExFalso of proof*ty
  | Hole

let rec proof_to_term (p : proof) : lam =
  match p with
  | Abstraction (v, t, p) -> Abstraction (v, t, proof_to_term p)
  | Application (p1, p2) -> Application (proof_to_term p1, proof_to_term p2)
  | Variable v -> Variable v
  | ExFalso (p, t) -> ExFalso (proof_to_term p, t)
  | Hole -> raise IncompleteProof