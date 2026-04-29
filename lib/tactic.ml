open Type

type tactic = 
  | Goal of ty
  | Qed
  | ShowProof

  | Exact of string
  | Trivial
  | Intro of string option
  | Intros of string list
  | Apply of string
(* TODO: search the arguments of the constructors *) 
  | Cut
  | ExFalso
  | Destruct
  | Absurd
  | Admit