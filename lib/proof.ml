open Exceptions
open Term
open Type

type proof =
  | Abstraction of string*ty*proof ref
  | Application of proof ref*proof ref
  | Variable of string
  | ExFalso of proof ref*ty
  | Hole

type hyp = (string * ty) list 
type subgoal = proof ref * ty * hyp
type subgoals = subgoal list

let rec proof_to_term (p : proof) : lam =
  match p with
  | Abstraction (v, t, p) -> Abstraction (v, t, proof_to_term !p)
  | Application (p1, p2) -> Application (proof_to_term !p1, proof_to_term !p2)
  | Variable v -> Variable v
  | ExFalso (p, t) -> ExFalso (proof_to_term !p, t)
  | Hole -> raise IncompleteProof