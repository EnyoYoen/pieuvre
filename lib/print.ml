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