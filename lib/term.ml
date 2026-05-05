open Type

type lam =
  | Abstraction of string*ty*lam
  | Application of lam*lam
  | Variable of string
  | ExFalso of lam*ty
  | Admit
  | True
  | Uple of lam*lam
  | First of lam
  | Second of lam
  | Left of lam*ty
  | Right of lam*ty
  | Case of lam*lam*lam

type alpha_env = (string * string) list

let rec alpha_aux (env : alpha_env) (t1 : lam) (t2 : lam) =
  match t1, t2 with
  | Application (l11, l12), Application (l21, l22) ->
    alpha_aux env l11 l21 && alpha_aux env l12 l22
  | Variable v1, Variable v2 -> (
      match List.assoc_opt v1 env with
      | Some mapped -> mapped = v2
      | None ->
        if List.exists (fun (_, right) -> right = v2) env
        then false
        else v1 = v2
    )
  | ExFalso (l1', _), ExFalso (l2', _) ->
    alpha_aux env l1' l2'
  | Abstraction (v1, _, l1'), Abstraction (v2, _, l2') ->
    alpha_aux ((v1, v2) :: env) l1' l2'
  | Admit, Admit -> true
  | True, True -> true
  | Uple (l11, l12), Uple (l21, l22) ->
    alpha_aux env l11 l21 && alpha_aux env l12 l22
  | First l1', First l2' ->
    alpha_aux env l1' l2'
  | Second l1', Second l2' ->
    alpha_aux env l1' l2'
  | Left (l1', _), Left (l2', _) ->
    alpha_aux env l1' l2'
  | Right (l1', _), Right (l2', _) ->
    alpha_aux env l1' l2'
  | Case (l11, l12, l13), Case (l21, l22, l23) ->
    alpha_aux env l11 l21 && alpha_aux env l12 l22 && alpha_aux env l13 l23
  | _, _ -> false

let alpha (l1 : lam) (l2 : lam) =
  alpha_aux [] l1 l2

let rec subst (l : lam) (v : string) (s : lam) =
  match l with
  | Abstraction (p, t, b) ->
    if p = v then
      Abstraction (p, t, b)
    else
      Abstraction (p, t, subst b v s)
  | Application (l1, l2) ->
    Application (subst l1 v s, subst l2 v s)
  | Variable var ->
    if var = v then s else Variable var
  | ExFalso (l', ty) ->
    ExFalso (subst l' v s, ty)
  | Admit -> Admit
  | True -> True
  | Uple (l1, l2) -> Uple (subst l1 v s, subst l2 v s)
  | First l' -> First (subst l' v s)
  | Second l' -> Second (subst l' v s)
  | Left (l', t) -> Left (subst l' v s, t)
  | Right (l', t) -> Right (subst l' v s, t)
  | Case (m, n, n') -> Case (subst m v s, subst n v s, subst n' v s)

let rec betastep (l : lam) =
  match l with
  | Variable _ -> None
  | Abstraction (p, t, b) -> (
    match betastep b with
    | None -> None
    | Some b' -> Some (Abstraction (p, t, b'))
  )
  | ExFalso (b, t) -> (
    match betastep b with
    | None -> None
    | Some b' -> Some (ExFalso (b', t))
  )
  | Application (l1, l2) -> (
    match betastep l1 with
    | Some l1' -> Some (Application (l1', l2))
    | None -> (
      match betastep l2 with
      | Some l2' -> Some (Application (l1, l2'))
      | None -> (
        match l1 with
        | Abstraction (param, _, body) -> Some (subst body param l2)
        | _ -> None
      )
    )
  )
  | Admit -> None
  | True -> None
  | Uple (l1, l2) -> (
    match betastep l1 with
    | Some l1' -> Some (Uple (l1', l2))
    | None -> (
      match betastep l2 with
      | Some l2' -> Some (Uple (l1, l2'))
      | None -> None
    )
  )
  | First l' -> (
    match betastep l' with
    | None -> (
      match l' with
      | Uple (l1, _) -> Some l1
      | _ -> None
    )
    | Some l'' -> Some (First l'')
  )
  | Second l' -> (
    match betastep l' with
    | None -> (
      match l' with
      | Uple (_, l2) -> Some l2
      | _ -> None
    )
    | Some l'' -> Some (Second l'')
  )
  | Left (b, t) -> (
    match betastep b with
    | None -> None
    | Some b' -> Some (Left (b', t))
  )
  | Right (b, t) -> (
    match betastep b with
    | None -> None
    | Some b' -> Some (Right (b', t))
  )
  | Case (m, n1, n2) -> (
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
          | _ -> None
        )
      )
    )
  )