open Type

type lam =
  | Abstraction of
      string * ty * lam (* name of argument, type of argument and body *)
  | Application of lam * lam (* term we apply to, the term as argument *)
  | Variable of string (* name of variable *)
  | ExFalso of
      lam * ty (* term that should type to false, type we want from this *)
  | Admit (* from admit tactic to admit a proof *)
  | True
  | Uple of lam * lam
  | First of lam
  | Second of lam
  | Left of lam * ty (* Sum type (term, type) *)
  | Right of lam * ty (* Sum type (type, term) *)
  | Case of lam * lam * lam (* Case m is A then n m else m is B then n' m *)

(* environment for alpha-conversion, we associate the varaible to the renaming *)
type alpha_env = (string * string) list

(* Checks recursively if the terms are alpha-convertible *)
let rec alpha_aux (env : alpha_env) (t1 : lam) (t2 : lam) =
  match (t1, t2) with
  | Application (l11, l12), Application (l21, l22) ->
      alpha_aux env l11 l21 && alpha_aux env l12 l22
  | Variable v1, Variable v2 -> (
      (* We check if the variables are already associated, if not check if they are the same and not already associated to another variable *)
      match List.assoc_opt v1 env with
      | Some mapped -> mapped = v2
      | None ->
          if List.exists (fun (_, right) -> right = v2) env then false
          else v1 = v2)
  | ExFalso (l1', _), ExFalso (l2', _) ->
      alpha_aux env l1' l2' (* We don't care about the type *)
  | Abstraction (v1, _, l1'), Abstraction (v2, _, l2') ->
      (* v1 can be renamed to v2 *)
      alpha_aux ((v1, v2) :: env) l1' l2'
  | Admit, Admit -> true
  | True, True -> true
  | Uple (l11, l12), Uple (l21, l22) ->
      alpha_aux env l11 l21 && alpha_aux env l12 l22
  | First l1', First l2' -> alpha_aux env l1' l2'
  | Second l1', Second l2' -> alpha_aux env l1' l2'
  | Left (l1', _), Left (l2', _) -> alpha_aux env l1' l2'
  | Right (l1', _), Right (l2', _) -> alpha_aux env l1' l2'
  | Case (l11, l12, l13), Case (l21, l22, l23) ->
      alpha_aux env l11 l21 && alpha_aux env l12 l22 && alpha_aux env l13 l23
  | _, _ -> false

(* Alpha-conversion function *)
let alpha (l1 : lam) (l2 : lam) = alpha_aux [] l1 l2

(* Counter for generating fresh variables *)
let fresh_var_counter = ref 0

let fresh_var () =
  let i = !fresh_var_counter in
  fresh_var_counter := i + 1;
  "_v" ^ string_of_int i

(* Compute the set of free variables in a term *)
let rec free_vars (l : lam) =
  match l with
  | Variable v -> [ v ]
  | Abstraction (p, _, b) -> List.filter (fun x -> x <> p) (free_vars b)
  | Application (l1, l2) -> free_vars l1 @ free_vars l2
  | ExFalso (l', _) -> free_vars l'
  | Admit -> []
  | True -> []
  | Uple (l1, l2) -> free_vars l1 @ free_vars l2
  | First l' -> free_vars l'
  | Second l' -> free_vars l'
  | Left (l', _) -> free_vars l'
  | Right (l', _) -> free_vars l'
  | Case (m, n, n') -> free_vars m @ free_vars n @ free_vars n'

(* Rename a variable p to p' throughout a term *)
let rec rename_var (l : lam) (p : string) (p' : string) =
  match l with
  | Variable v -> if v = p then Variable p' else Variable v
  | Abstraction (a, t, b) ->
      if a = p then Abstraction (a, t, b)
      else Abstraction (a, t, rename_var b p p')
  | Application (l1, l2) -> Application (rename_var l1 p p', rename_var l2 p p')
  | ExFalso (l', t) -> ExFalso (rename_var l' p p', t)
  | Admit -> Admit
  | True -> True
  | Uple (l1, l2) -> Uple (rename_var l1 p p', rename_var l2 p p')
  | First l' -> First (rename_var l' p p')
  | Second l' -> Second (rename_var l' p p')
  | Left (l', t) -> Left (rename_var l' p p', t)
  | Right (l', t) -> Right (rename_var l' p p', t)
  | Case (m, n, n') ->
      Case (rename_var m p p', rename_var n p p', rename_var n' p p')

(* We replace recursively the variable v by the term s in the term l.
   To avoid variable capture, when substituting under an abstraction,
   if the bound variable appears free in the substitution term,
   we rename the bound variable to a fresh one first. *)
let rec subst (l : lam) (v : string) (s : lam) =
  match l with
  | Abstraction (p, t, b) ->
      if p = v then
        (* The variable we want to substitute is now bound, so stop *)
        Abstraction (p, t, b)
      else if List.mem p (free_vars s) then
        (* Variable capture risk, we rename p to a fresh variable *)
        let p' = fresh_var () in
        let b' = rename_var b p p' in
        Abstraction (p', t, subst b' v s)
      else
        (* Safe to substitute in the body *)
        Abstraction (p, t, subst b v s)
  | Application (l1, l2) -> Application (subst l1 v s, subst l2 v s)
  | Variable var -> if var = v then s else Variable var
  | ExFalso (l', ty) -> ExFalso (subst l' v s, ty)
  | Admit -> Admit
  | True -> True
  | Uple (l1, l2) -> Uple (subst l1 v s, subst l2 v s)
  | First l' -> First (subst l' v s)
  | Second l' -> Second (subst l' v s)
  | Left (l', t) -> Left (subst l' v s, t)
  | Right (l', t) -> Right (subst l' v s, t)
  | Case (m, n, n') -> Case (subst m v s, subst n v s, subst n' v s)

(* One step of beta-reduction, return None if in normal form. We priorize reducing the deepest left-most redex *)
let rec betastep (l : lam) =
  match l with
  | Variable _ -> None (* Normal form *)
  | Abstraction (p, t, b) -> (
      (* Reduce inside if possible *)
      match betastep b with
      | None -> None
      | Some b' -> Some (Abstraction (p, t, b')))
  | ExFalso (b, t) -> (
      (* Reduce inside if possible *)
      match betastep b with
      | None -> None
      | Some b' -> Some (ExFalso (b', t)))
  | Application (l1, l2) -> (
      (* Reduce l1, then l2, then reduce the application if l1 is an abstraction (redex) *)
      match betastep l1 with
      | Some l1' -> Some (Application (l1', l2))
      | None -> (
          match betastep l2 with
          | Some l2' -> Some (Application (l1, l2'))
          | None -> (
              match l1 with
              | Abstraction (param, _, body) -> Some (subst body param l2)
              | _ -> None)))
  | Admit -> None
  | True -> None
  | Uple (l1, l2) -> (
      (* Reduce l1, then l2 *)
      match betastep l1 with
      | Some l1' -> Some (Uple (l1', l2))
      | None -> (
          match betastep l2 with
          | Some l2' -> Some (Uple (l1, l2'))
          | None -> None))
  | First l' -> (
      (* Reduce l', if not possible check if l' is a tuple and reduce accordingly *)
      match betastep l' with
      | None -> ( match l' with Uple (l1, _) -> Some l1 | _ -> None)
      | Some l'' -> Some (First l''))
  | Second l' -> (
      (* Reduce l', if not possible check if l' is a tuple and reduce accordingly *)
      match betastep l' with
      | None -> ( match l' with Uple (_, l2) -> Some l2 | _ -> None)
      | Some l'' -> Some (Second l''))
  | Left (b, t) -> (
      match betastep b with None -> None | Some b' -> Some (Left (b', t)))
  | Right (b, t) -> (
      match betastep b with None -> None | Some b' -> Some (Right (b', t)))
  | Case (m, n1, n2) -> (
      (* Reduce m, then n1, then n2, then if m is left or right and n1 or n2 an application, we apply m to n1 or n2 *)
      match betastep m with
      | Some m' -> Some (Case (m', n1, n2))
      | None -> (
          match betastep n1 with
          | Some n1' -> Some (Case (m, n1', n2))
          | None -> (
              match betastep n2 with
              | Some n2' -> Some (Case (m, n1, n2'))
              | None -> (
                  match m with
                  | Left (b, _) -> Some (Application (n1, b))
                  | Right (b, _) -> Some (Application (n2, b))
                  | _ -> None))))
