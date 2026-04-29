open Exceptions
open Proof
open Tactic
open Type
open Print
open Infer

let var_counter = ref 0
let hyp_counter = ref 0

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

let process_intro (sg : subgoal) (env : gam) (h : string option) =
  let (proof, goal, hyps) = sg in
  match goal with 
  | Implication (t1, t2) -> 
    let hyp_name = (
      match h with
      | None -> 
        let i = !hyp_counter in 
        hyp_counter := (!hyp_counter + 1);
        "H" ^ (string_of_int i)
      | Some n -> n
    ) in (
      match List.assoc_opt hyp_name env with 
      | Some _ -> raise HypothesisExists 
      | None -> (
        let varname = "x" ^ (string_of_int !var_counter) in
        var_counter := (!var_counter + 1);
        let new_proof = ref Hole in
        proof := Abstraction (varname, t1, new_proof);
        (new_proof, t2, (hyp_name, t1) :: hyps)
      )
    )
  | _ -> raise IntroOnNonImplication

let process_tactic (t : tactic) (sg : subgoal) (env : gam) =
  match t with
  | Qed -> true, [], env
  | Exact h -> 
    let env' = (process_exact sg env h) in
    (false, [], env')
  | Intro h -> 
    let sg' = (process_intro sg env h) in
    (false, [sg'], env)
  | _ -> raise NotImplemented

let rec process_until_qed (tactics : tactic list) (sgs : subgoals) (env : gam) =
  match tactics with 
  | [] -> env
  | t :: tl -> (
    match sgs with 
    | [] -> (
      match t with 
      | Qed -> env
      | _ -> raise NoGoal
    )
    | sg :: sgsr -> (
      let (b, sgs', env') = (process_tactic t sg env) in 
      if b then
        if sgs' <> [] then
          raise ProofNotFinished
        else (
          print_subgoal sgs';
          env'
        )
      else (
        print_subgoal sgs';
        process_until_qed tl (sgs' @ sgsr) env'
      )
    )
  )
  
let process_proof (tactics : tactic list) = 
  match tactics with 
  | [] -> ()
  | goalt :: tl -> 
    match goalt with
    | Goal goal -> (
      let proof = ref Hole in
      let subgoal = [(proof, goal, [])] in
      print_subgoal subgoal;
      let e = process_until_qed tl subgoal [] in
      let term = proof_to_term !proof in
      if not (typecheck e term goal) then 
        raise IncorrectProof
    )
    | _ -> raise NoGoal

let process_proofs (tl : tactic list) = 
  print_tactics tl;
  process_proof tl (* Loop through all proofs till empty list *)