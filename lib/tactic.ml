open Type
open Term

(* List of possible tactics: *)
type tactic =
  | Goal of ty (* Start a proof with a goal type *)
  | Qed (* Try to finish the current proof *)
  | ShowProof (* We show the actual proof *)
  | Exact of string (* Complete the subgoal with a given hypothesis name *)
  | ExactTerm of lam (* Complete the subgoal with a given term *)
  | Trivial (* Check the hypotheses to find one fitting the goal *)
  | Intro of string option (* Add an hypothesis when the goal is an Implication (introduction rule of ->) with a name, or an automatic one when none given *)
  | Intros of string list (* Apply intro as much as possible, naming the hypotheses with the given names or automatically when running out of given names *)
  | Apply of string (* Try to use the given hypothesis (has to be an implication with the final type being the goal) and create intermediary subgoals (eliminination rule of ->)  *)
  | Cut of ty (* If the goal is B, cut A creates two subgoals: A -> B and A *)
  | ExFalso (* Try to prove False to get the goal (ex falso quodlibet) *)
  | Destruct of string (* Destruct the given hypothesis H: H = False, proves the current subgoal, H = A /\ B -> creates two hypotheses for A and B, 
                          H = A \/ B -> creates two subgoals, one with H = A, one with H = B (special case of elimination rule  of A \/ B) *)
  | Absurd of ty (* A the given type, creates two subgoal to prove A and ~A and conclude by exfalso *)
  | Admit (* Admit the current subgoal *)
  | Split (* If the subgoal is A /\ B, creates two subgoals to prove A and B (introduction rule of /\) *)
  | Left (* If the subgoal is A \/ B, creates a subgoal to prove A (left introduction rule of \/) *)
  | Right (* If the subgoal is A \/ B, creates a subgoal to prove B (right introduction rule of \/) *)
