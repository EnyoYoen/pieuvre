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

let rec create_intermediate_subgoals (hyps : hyp) (env : gam) (types : ty list) (goal : ty) (proof : proof ref) =
  match types with 
  | [] -> raise NotImplemented
  | [_] -> 
    let varname = "x" ^ (string_of_int !var_counter) in
    var_counter := (!var_counter + 1);
    proof := (Variable varname);
    [], (varname, goal) :: env
  | t :: tl ->
    let new_proof = ref Hole in
    let next_iter_proof = ref Hole in
    proof := Application (next_iter_proof, new_proof);
    let sgs, env' = (create_intermediate_subgoals hyps env tl (Implication (t, goal)) next_iter_proof) in 
    (new_proof, t, hyps) :: sgs, env'

let rec build_type_list (goal : ty) = 
  match goal with
  | Implication (t1, t2) -> t1 :: (build_type_list t2)
  | _ -> [goal]

let process_apply (sg : subgoal) (env : gam) (h : string) =
  let (proof, _, hyps) = sg in
  match List.assoc_opt h hyps with 
  | None -> raise HypothesisNotFound
  | Some ty ->
    let types = (build_type_list ty) in 
    match types with 
    | [] -> raise HypothesisNotImplication
    | [_] -> raise HypothesisNotImplication  
    | t :: _ -> create_intermediate_subgoals hyps env types t proof  

let process_tactic (t : tactic) (sg : subgoal) (env : gam) =
  match t with
  | Exact h -> 
    let env' = (process_exact sg env h) in
    (false, [], env')
  | Intro h -> 
    let sg' = (process_intro sg env h) in
    (false, [sg'], env)
  | Apply h -> 
    let sgs, env' = (process_apply sg env h) in
    (false, sgs, env')
  | _ -> raise NotImplemented

let rec process_until_qed (tactics : tactic list) (sgs : subgoals) (env : gam) =
  match tactics with 
  | [] -> env
  | Qed :: _ -> (
    if sgs <> [] then
      raise ProofNotFinished
    else
      env
  )
  | t :: tl -> (
    match sgs with 
    | [] -> raise NoGoal
    | sg :: sgsr -> (
      let (b, sgs', env') = (process_tactic t sg env) in 
      let remaining = sgs' @ sgsr in
      if remaining <> [] then
        print_subgoal remaining;
      if b && remaining <> [] then
        raise ProofNotFinished
      else
        process_until_qed tl remaining env'
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
      if typecheck e term goal then (
        print_string "Proof successful! Term: ";
        print_lam term;
        print_newline ()
      ) else
        raise IncorrectProof
    )
    | _ -> raise NoGoal

let process_proofs (tl : tactic list) = 
  print_tactics tl;
  process_proof tl (* Loop through all proofs till empty list *)