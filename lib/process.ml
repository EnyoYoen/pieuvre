open Exceptions
open Term
open Proof
open Tactic
open Type
open Print
open Infer

let var_counter = ref 0
let hyp_counter = ref (-1)

let new_hyp () =
  let i = !hyp_counter in 
  hyp_counter := (!hyp_counter + 1);
  "H" ^ (if i = -1 then "" else (string_of_int i))

let new_var () = 
  let i = !var_counter in
  var_counter := (!var_counter + 1);
  "x" ^ (string_of_int i)

let process_exact (sg : subgoal) (h : string) =
  let (proof, goal, hyps) = sg in
  match List.assoc_opt h hyps with 
  | None -> raise HypothesisNotFound
  | Some (ty, vn) -> 
    if ty = goal then (
      proof := (Variable vn)
    ) else
      raise HypothesisMismatch 

let process_exact_term (sg : subgoal) (env : gam) (l : lam) = 
  let (proof, goal, _) = sg in
  let pot_proof = term_to_proof l in
  if typecheck env l goal then
    proof := pot_proof
  else
    raise TermMismatch

let process_trivial (sg : subgoal) =
  let (proof, goal, hyps) = sg in
  match List.find_opt (fun (_, (ty, _)) -> ty = goal) hyps with
  | None -> raise HypothesisNotFound
  | Some (_, (_, vn)) ->
    proof := Variable vn

let hypothesis_name_exists hyp_name env hyps =
  List.mem_assoc hyp_name env || List.mem_assoc hyp_name hyps

let process_intro (sg : subgoal) (env : gam) (h : string option) =
  let (proof, goal, hyps) = sg in
  match goal with 
  | Implication (t1, t2) -> 
    let hyp_name = (
      match h with
      | None -> new_hyp ()
      | Some n -> n
    ) in (
      if hypothesis_name_exists hyp_name env hyps then
        raise HypothesisExists
      else (
        let varname = new_var () in 
        let new_proof = ref Hole in
        proof := Abstraction (varname, t1, new_proof);
        (new_proof, t2, (hyp_name, (t1, varname)) :: hyps)
      )
    )
  | _ -> raise IntroOnNonImplication

let rec validate_intros goal env hyps seen_names = function
  | [] -> ()
  | h :: tl ->
    if List.mem h seen_names || hypothesis_name_exists h env hyps then
      raise HypothesisExists
    else
      match goal with
      | Implication (_, next_goal) ->
        validate_intros next_goal env hyps (h :: seen_names) tl
      | _ -> raise IntroOnNonImplication
  
let rec deep_intro (sg: subgoal) =
  let (proof, goal, hyps) = sg in 
  match goal with 
  | Implication (t1, t2) -> 
    let varname = new_var () in
    let new_proof = ref Hole in
    proof := Abstraction (varname, t1, new_proof);
    deep_intro (new_proof, t2, (new_hyp (), (t1, varname)) :: hyps)
  | _ -> sg

let process_intros (sg : subgoal) (env : gam) (hs : string list) =
  if hs = [] then
    deep_intro sg
  else
    let (_, goal, hyps) = sg in
    validate_intros goal env hyps [] hs;
    List.fold_left (fun sg h -> process_intro sg env (Some h)) sg hs

let create_intermediate_subgoals (hyps : hyp) (vn : string) (args : ty list) (proof : proof ref) =
  let arg_refs = List.map (fun _ -> ref Hole) args in
  let root = ref (Variable vn) in
  let final_root = List.fold_left (fun acc_ref arg_ref ->
    let new_root = ref Hole in
    new_root := Application (acc_ref, arg_ref);
    new_root
  ) root arg_refs in
  proof := !final_root;
  List.map2 (fun r t -> (r, t, hyps)) arg_refs args

let rec build_type_list (goal : ty) = 
  match goal with
  | Implication (t1, t2) -> t1 :: (build_type_list t2)
  | _ -> [goal]

let process_apply (sg : subgoal) (h : string) =
  let (proof, goal, hyps) = sg in
  match List.assoc_opt h hyps with 
  | None -> raise HypothesisNotFound
  | Some (ty, vn) ->
    let types = List.rev (build_type_list ty) in
    match types with
    | [] -> raise HypothesisNotImplication
    | [_] -> raise HypothesisNotImplication
    | t :: tl ->
      if t <> goal then
        raise HypothesisMismatch
      else
        create_intermediate_subgoals hyps vn (List.rev tl) proof

let process_cut (sg : subgoal) (t : ty) =
  let (proof, goal, hyps) = sg in
  let proof_cut = ref Hole in
  let proof_impl = ref Hole in
  proof := Application (proof_cut, proof_impl);
  [(proof_cut, Implication (t, goal), hyps); (proof_impl, t, hyps)]

let process_exfalso (sg : subgoal) =
  let (proof, goal, hyps) = sg in
  let proof_exfalso = ref Hole in
  proof := ExFalso (proof_exfalso, goal);
  [(proof_exfalso, False, hyps)]

let process_destruct (sg : subgoal) =
  let (proof, goal, hyps) = sg in
  let proof_destruct = ref Hole in
  proof := ExFalso (proof_destruct, goal);
  [(proof_destruct, False, hyps)]

let process_absurd (sg : subgoal) (env : gam) (t : ty) =
  let (proof, goal, hyps) = sg in
  let proof_prop = ref Hole in
  let proof_neg_prop = ref Hole in
  let varname = new_var () in
  proof := ExFalso(ref (Application (ref (Application (ref (Variable varname), proof_prop)), proof_neg_prop)), goal);
  let not_t = Implication (t, False) in
  [(proof_prop, t, hyps); (proof_neg_prop, not_t, hyps)], (varname, Implication (t, Implication (not_t, False))) :: env

let process_admit (sg : subgoal) =
  let (proof, _, _) = sg in
  proof := Admit

let process_split (sg: subgoal) = 
  let (proof, goal, hyps) = sg in
  match goal with 
  | Conjunction (t1, t2) ->
    let proof_left = ref Hole in
    let proof_right = ref Hole in
    proof := Uple (proof_left, proof_right); 
    [(proof_left, t1, hyps); (proof_right, t2, hyps)]
  | _ -> raise HypothesisNotConjunction

let process_left (sg: subgoal) =
  let (proof, goal, hyps) = sg in
  match goal with 
  | Disjunction (t1, t2) ->
    let proof_left = ref Hole in
    proof := Left (proof_left, t2); 
    [(proof_left, t1, hyps)]
  | _ -> raise HypothesisNotDisjunction

let process_right (sg: subgoal) =
  let (proof, goal, hyps) = sg in
  match goal with 
  | Disjunction (t1, t2) ->
    let proof_right = ref Hole in
    proof := Right (proof_right, t1); 
    [(proof_right, t2, hyps)]
  | _ -> raise HypothesisNotDisjunction

let process_tactic (t : tactic) (sg : subgoal) (env : gam) =
  match t with
  | Exact h -> 
    process_exact sg h;
    (false, [], env)
  | ExactTerm l ->
    process_exact_term sg env l;
    (false, [], env)
  | Trivial ->
    process_trivial sg;
    (false, [], env)
  | Intro h -> 
    let sg' = (process_intro sg env h) in
    (false, [sg'], env)
  | Intros hs -> 
    let sg' = (process_intros sg env hs) in
    (false, [sg'], env)
  | Apply h -> 
    let sgs = (process_apply sg h) in
    (false, sgs, env)
  | Cut t ->
    let sgs = (process_cut sg t) in
    (false, sgs, env)
  | ExFalso ->
    let sgs = (process_exfalso sg) in
    (false, sgs, env)
  | Destruct ->
    let sgs = (process_destruct sg) in
    (false, sgs, env)
  | Absurd t ->
    let (sgs, env') = (process_absurd sg env t) in
    (false, sgs, env')
  | Admit ->
    process_admit sg;
    (false, [], env)
  | Split -> 
    let sgs = (process_split sg) in
    (false, sgs, env)
  | Left -> 
    let sgs = (process_left sg) in
    (false, sgs, env)
  | Right -> 
    let sgs = (process_right sg) in
    (false, sgs, env)
  | ShowProof ->
    (false, [], env) (* TODO *)
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


  
type interactive_session = proof ref option * subgoals * gam * ty option

type interactive_step =
  | StepContinue of interactive_session * subgoals option
  | StepFinished of lam
  | StepError of string

let process_interactive_tactic (session : interactive_session) (t : tactic) : interactive_step =
  let (proof, sgs, env, goal) = session in
  try
    match t with
    | Goal g ->
      (match proof with
       | Some _ -> StepError "A goal is already active."
       | None ->
         let new_proof = ref Hole in
         let new_sgs = [(new_proof, g, [])] in
         StepContinue ((Some new_proof, new_sgs, env, Some g), Some new_sgs))
    | Qed ->
      (match proof with
       | None -> StepError "No proof in progress."
       | Some proof ->
         if sgs <> [] then
           StepError "Proof not finished."
         else
           let term = proof_to_term !proof in
           match goal with
           | None -> StepError "No goal."
           | Some goal ->
             if Infer.typecheck env term goal then
               StepFinished term
             else
               StepError "Typecheck of the final term failed: pieuvre bug."
      )
    | ShowProof -> StepContinue (session, None)
    | _ ->
      (match sgs with
       | [] -> StepError "No goal. Start with 'Goal <goal>.' ."
       | sg :: r ->
         let (_, new_sgs, new_env) = process_tactic t sg env in
         let remaining = new_sgs @ r in
         StepContinue ((proof, remaining, new_env, goal), Some remaining))
  with e ->
    StepError (Printexc.to_string e)