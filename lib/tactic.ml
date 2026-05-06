open Type
open Term

type tactic = 
  | Goal of ty
  | Qed
  | ShowProof

  | ExactTerm of lam
  | Exact of string
  | Trivial
  | Intro of string option
  | Intros of string list
  | Apply of string
  | Cut of ty 
  | ExFalso
  | Destruct
  | Absurd of ty
  | Admit