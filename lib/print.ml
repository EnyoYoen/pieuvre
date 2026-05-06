open Term
open Type
open Tactic
open Proof

let rec print_type (t : ty) =
  match t with
  | Base b -> print_string b
  | Implication (t, False) -> 
    print_string "~";
    (match t with
     | Implication _ -> print_string "("; print_type t; print_string ")"
     | _ -> print_type t)
  | Implication (t1, t2) -> 
    (
      match t1 with
      | Implication _ -> print_string "("; print_type t1; print_string ")"
      | _ -> print_type t1;
    );
    print_string " -> ";
    print_type t2
  | False -> print_string "False"
  | True -> print_string "True"
  | Conjunction (t1, t2) ->
    print_string "(";
    print_type t1;
    print_string " /\\ ";
    print_type t2;
    print_string ")"
  | Disjunction (t1, t2) ->
    print_string "(";
    print_type t1;
    print_string " \\/ ";
    print_type t2;
    print_string ")"

let rec print_lam (l : lam) =
  match l with
  | Abstraction (v, t, l') -> 
    print_string ("fun (" ^ v ^ " : ");
    print_type t;
    print_string ") => ";
    print_lam l'
  | Application (l1, l2) ->
    print_string "(";
    print_lam l1;
    print_string ") (";
    print_lam l2;
    print_string ")"
  | Variable v -> 
    print_string v
  | ExFalso (l, t) ->
    print_string "exf(";
    print_lam l;
    print_string " : ";
    print_type t;
    print_string ")"
  | Admit -> print_string "admit"
  | True -> print_string "I"
  | Uple (l1, l2) ->
    print_string "(";
    print_lam l1;
    print_string ", ";
    print_lam l2;
    print_string ")"
  | First l' ->
    print_string "fst(";
    print_lam l';
    print_string ")"
  | Second l' ->
    print_string "snd(";
    print_lam l';
    print_string ")"
  | Left (l', t) ->
    print_string "ig(";
    print_lam l';
    print_string ", ";
    print_type t;
    print_string ")"
  | Right (l', t) ->
    print_string "id(";
    print_lam l';
    print_string ", ";
    print_type t;
    print_string ")"
  | Case (m, n, n') ->
    print_string "case(";
    print_lam m;
    print_string ", ";
    print_lam n;
    print_string ", ";
    print_lam n';
    print_string ")"

let rec reduce_aux (s : int) (t : lam) =  
    print_lam t;
    print_newline ();
    if s <= 0 then
      print_endline "[reduction stopped after 1000 steps: possible divergence]"
    else
    match betastep t with
    | Some next -> reduce_aux (s - 1) next
    | None -> ()

let reduce (t : lam) =
  reduce_aux 1000 t

let print_tactic (t : tactic) = 
  (
    match t with 
    | Goal t -> 
      print_string "Goal: ";
      print_type t
    | Qed -> print_string "Qed"
    | ShowProof -> print_string "Show Proof"
    | ExactTerm t -> print_string "exact "; print_lam t;
    | Exact h -> print_string ("exact " ^ h)
    | Trivial -> print_string "trivial"
    | Intro h -> print_string ("intro " ^ (match h with Some s -> s | None -> ""))
    | Intros hs -> print_string ("intros " ^ (String.concat " " hs))
    | Apply h -> print_string ("apply " ^ h)
    | Cut t -> print_string "cut "; print_type t
    | ExFalso -> print_string "exfalso"
    | Destruct h -> print_string ("destruct " ^ h)
    | Absurd t -> print_string "absurd "; print_type t
    | Admit -> print_string "admit"
    | Split -> print_string "split"
    | Left -> print_string "left"
    | Right -> print_string "right"
  ); 
  print_string "."

let print_tactics (ts : tactic list) =
  List.iter (fun t -> print_tactic t; print_newline ()) ts;
  print_newline ()

let print_subgoal (sgs : subgoals) =
  match sgs with
  | [] -> 
    print_string "No more goals.";
    print_newline ();
    print_newline ()
  | (_, goal, hyps) :: _ -> 
    let len = List.length sgs in
    print_string (string_of_int len ^ " goal" ^ (if len > 1 then "s" else ""));
    print_newline ();
    List.iter (fun (h, (t, _)) -> 
      print_string ("  " ^ h ^ " : ");
      print_type t;
      print_newline ()
    ) hyps;
    print_string "============================";
    print_newline ();
    print_type goal;
    print_newline ();
    print_newline ()