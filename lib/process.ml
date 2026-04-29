open Exceptions
open Proof
open Tactic
open Type

type hyp = (string * ty) list 
type subgoal = proof ref * ty * hyp
type subgoals = subgoal list

let var_counter = ref 0

let process_exact (sg : subgoal) (env : gam) (h : string) =
  let (proof, goal, hyps) = sg in
  match List.assoc_opt h hyps with 
  | None -> raise HypothesisNotFound
  | Some ty -> 
    if ty = goal then 
      let varname = "x" ^ (string_of_int !var_counter) in
      var_counter := (!var_counter + 1);
      proof := (Variable varname);
      (varname, goal) :: env
    else
      raise HypothesisMismatch 

let process_tactic (t : tactic) (sg : subgoal) (env : gam) =
  match t with
  | Qed -> true, [], env
  | Exact h -> 
    let env' = (process_exact sg env h) in
    (false, [], env')
  | _ -> raise NotImplemented

let rec process_until_qed (tactics : tactic list) (sgs : subgoals) (env : gam) =
  match tactics with 
  | [] -> ()
  | t :: tl -> (
    match sgs with 
    | [] -> (
      match t with 
      | Qed -> ()
      | _ -> raise ProofNotFinished
    )
    | sg :: sgsr -> (
      let (b, sgs', env') = (process_tactic t sg env) in 
      if not b then process_until_qed tl (sgs' @ sgsr) env'
    )
  )
let process_proof (tactics : tactic list) = 
  match tactics with 
  | [] -> ()
  | goalt :: tl -> 
    match goalt with
    | Goal goal -> (
      let proof = Hole in
      let subgoal = [(ref proof, goal, [])] in
      process_until_qed tl subgoal []
    )
    | _ -> raise NoGoal

let process_proofs (tl : tactic list) = 
  process_proof tl (* Loop through all proofs till empty list *)