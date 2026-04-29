open Type

type tactic = 
  | Goal of ty
  | Exact of string
  | Intro of string option
  | Intros of string list
  | Apply of string
(* TODO: search the arguments of the constructors *) 
  | Cut
  | ExFalso
  | Destruct
  | Absurd
  | Admit