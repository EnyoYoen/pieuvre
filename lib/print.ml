open Term
open Type

let rec print_type (t : ty) =
  match t with
  | Base b -> print_string b
  | Implication (t, False) -> 
    print_string "~";
    print_type t
  | Implication (t1, t2) -> 
    print_type t1; 
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