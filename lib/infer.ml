open Type
open Term
open Exceptions

let rec infer (env : gam) (l : lam) = 
  match l with
  | Variable v -> (try List.assoc v env with Not_found -> raise TypeError)
  | Application (l1, l2) -> (
    match infer env l1 with
    | Implication (t1, t2) -> check env l2 t1; t2
    | _ -> raise TypeError
  )
  | Abstraction (v, t, l) -> Implication (t, infer ((v, t) :: env) l)
  | ExFalso (l, t) -> check env l False; t 
and check (env : gam) (l : lam) (t : ty) =
  match l, t with
  | Abstraction (v, t_arg, body), Implication (t_dom, t_cod) ->
    if t_arg = t_dom then
      check ((v, t_dom) :: env) body t_cod
    else
      raise TypeError
  | _ ->
    if infer env l = t then () else raise TypeError

let typecheck (env : gam) (l : lam) (t : ty) =
  try
    check env l t;
    true
  with
  | TypeError -> false