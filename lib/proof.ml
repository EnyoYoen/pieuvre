open Exceptions
open Term
open Type

type proof =
  | Abstraction of string*ty*proof ref
  | Application of proof ref*proof ref
  | Variable of string
  | ExFalso of proof ref*ty
  | Admit
  | True
  | Uple of proof ref*proof ref
  | First of proof ref
  | Second of proof ref
  | Left of proof ref*ty
  | Right of proof ref*ty
  | Case of proof ref*proof ref*proof ref
  | Hole

type hyp = (string * (ty * string)) list 
type subgoal = proof ref * ty * hyp
type subgoals = subgoal list

let rec proof_to_term (p : proof) : lam =
  match p with
  | Abstraction (v, t, p) -> Abstraction (v, t, proof_to_term !p)
  | Application (p1, p2) -> Application (proof_to_term !p1, proof_to_term !p2)
  | Variable v -> Variable v
  | ExFalso (p, t) -> ExFalso (proof_to_term !p, t)
  | Admit -> Admit
  | True -> True
  | Uple (p1, p2) -> Uple (proof_to_term !p1, proof_to_term !p2)
  | First p -> First (proof_to_term !p)
  | Second p -> Second (proof_to_term !p)
  | Left (p, t) -> Left (proof_to_term !p, t)
  | Right (p, t) -> Right (proof_to_term !p, t)
  | Case (m, n, n') -> Case (proof_to_term !m, proof_to_term !n, proof_to_term !n')
  | Hole -> raise IncompleteProof

let rec term_to_proof (l : lam) : proof = 
  match l with
  | Abstraction (v, t, l) -> Abstraction (v, t, ref (term_to_proof l))
  | Application (l1, l2) -> Application (ref (term_to_proof l1), ref (term_to_proof l2))
  | Variable v -> Variable v
  | ExFalso (l, t) -> ExFalso (ref (term_to_proof l), t)
  | Admit -> Admit
  | True -> True
  | Uple (l1, l2) -> Uple (ref (term_to_proof l1), ref (term_to_proof l2))
  | First l -> First (ref (term_to_proof l))
  | Second l -> Second (ref (term_to_proof l))
  | Left (l, t) -> Left (ref (term_to_proof l), t)
  | Right (l, t) -> Right (ref (term_to_proof l), t)
  | Case (m, n, n') -> Case (ref (term_to_proof m), ref (term_to_proof n), ref (term_to_proof n'))