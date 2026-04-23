open Type

type lam =
  | Abstraction of string*ltype*lam
  | Application of lam*lam
  | Variable of string
  | ExFalso of lam*ltype

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

let reduce (_ : lam) = 
  ()