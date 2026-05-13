open Type
open Term
open Exceptions

(* Infer the type of a lambda term with the given environment *)
let rec infer (env : gam) (l : lam) =
  match l with
  | Variable v -> (
    (* We search the variable in the env, can't infer if not found *)
    match List.assoc v env with 
    | None  -> raise TypeError
    | Some t -> t)
  | Application (l1, l2) -> (
      (* We infer the type of the term we apply to, check that it is an Implication and check that the applicant is of the right type *)
      match infer env l1 with
      | Implication (t1, t2) ->
          check env l2 t1;
          t2
      | _ -> raise TypeError)
  | Abstraction (v, t, l) -> Implication (t, infer ((v, t) :: env) l)
  | ExFalso (l, t) ->
    (* The type of t has to be False (ex falso quodlibet) *)
    check env l False;
    t
  | Admit -> raise TypeError (* The final proof should not contain admits *)
  | True -> True
  | Uple (l1, l2) -> Conjunction (infer env l1, infer env l2)
  | First l' -> (
    (* First only works on uples *)
    match infer env l' with Conjunction (t1, _) -> t1 | _ -> raise TypeError)
  | Second l' -> (
    (* Second only works on uples *)
    match infer env l' with Conjunction (_, t2) -> t2 | _ -> raise TypeError)
  | Left (l', t) -> Disjunction (infer env l', t)
  | Right (l', t) -> Disjunction (t, infer env l')
  | Case (m, n, n') -> (
    (* m should be a disjunction A \/ B, and n, n' applications (A -> C, B -> C) so that we can "apply" m to n or n' *)
    match infer env m with
    | Disjunction (t1, t2) -> (
      match (infer env n, infer env n') with
      | Implication (t11, t12), Implication (t21, t22) ->
        if t11 = t1 && t21 = t2 && t12 = t22 then t12 else raise TypeError
      | _, _ -> raise TypeError)
    | _ -> raise TypeError)

(* Check that the given term has the given type using the environment *)
and check (env : gam) (l : lam) (t : ty) =
  match (l, t) with
  | Abstraction (v, ta, b), Implication (t1, t2) ->
    (* Check if the argument of the abstraction has the right type *)
    if ta = t1 then check ((v, t1) :: env) b t2 else raise TypeError
  | _ -> if infer env l = t then () else raise TypeError

(* Simple function that returns false instead of an error when checking fails *)
let typecheck (env : gam) (l : lam) (t : ty) =
  try
    check env l t;
    true
  with TypeError -> false
