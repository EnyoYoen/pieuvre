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
  | Variable v1, Variable v2 ->
    begin
      match List.assoc_opt v1 env with
      | Some mapped -> mapped = v2
      | None ->
        if List.exists (fun (_, right) -> right = v2) env
        then false
        else v1 = v2
    end
  | ExFalso (l1', _), ExFalso (l2', _) ->
    alpha_aux env l1' l2'
  | Abstraction (v1, _, l1'), Abstraction (v2, _, l2') ->
    alpha_aux ((v1, v2) :: env) l1' l2'
  | _, _ -> false

let alpha (l1 : lam) (l2 : lam) =
  alpha_aux [] l1 l2

let betastep (l : lam) = 
  Some (l)

let reduce (_ : lam) = 
  ()