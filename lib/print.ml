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
  match t with 
  | Goal t -> 
    print_string "Goal: ";
    print_type t
  | Qed -> print_string "Qed"
  | ShowProof -> print_string "ShowProof"
  | Exact h -> print_string ("Exact " ^ h)
  | Trivial -> print_string "Trivial"
  | Intro h -> print_string ("Intro " ^ (match h with Some s -> s | None -> ""))
  | Intros hs -> print_string ("Intros " ^ (String.concat " " hs))
  | Apply h -> print_string ("Apply " ^ h)
  | Cut -> print_string "Cut"
  | ExFalso -> print_string "ExFalso"
  | Destruct -> print_string "Destruct"
  | Absurd -> print_string "Absurd"
  | Admit -> print_string "Admit"

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