

let nom_fichier = ref ""

let recupere_entree () =
  Arg.parse
    []
    (fun s -> nom_fichier := s)
    "";
  try
    let where_from = match !nom_fichier with
      | "" -> stdin
      | s -> open_in s in
    let _ = Lexing.from_channel where_from in
    0
  with e -> (Printf.printf "probl�me de saisie\n"; raise e)


let execute _ =
  begin
    
  end

let run () =
  try
      let saisie = recupere_entree () in
	execute saisie; flush stdout
  with e -> raise e


let _ = run ()

