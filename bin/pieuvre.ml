open Lib

(* options de configuration du programme *)
let reduce = ref false          (* takes as input a lambda expression written in a file passed as an argument and displays the sequence of its reductions *)
let alpha = ref false           (* takes two lambda expressions as input, separated by the & character and written to a file passed as an argument, and returns true or false depending on whether they are alpha-convertible or not *)
let typecheck = ref false       (* takes as input a file containing a lambda expression and a type, separated by a colon (:), and ending with a period (.), and verifies that the typing relationship holds *)
let filename = ref ""           (* name of file to check *)

(* get and parse the input from stdin or a file *)
let get_input () =
  Arg.parse
    [
      ("-reduce", Arg.Set reduce, "Display the sequence of its reduction");
      ("-alpha", Arg.Set alpha, "Check alpha-convertibility");
      ("-typecheck", Arg.Set typecheck, "Check if the typing relationship hold");
    ]
    (fun s -> filename := s)
    "";
  try
    let where_from = match !filename with "" -> stdin | s -> open_in s in
    let lexbuf = Lexing.from_channel where_from in
    let parse () = Parser.main Lexer.token lexbuf in
    parse ()
  with e ->
    Printf.printf "Input problem\n";
    raise e

let run () =
  try
    let t = get_input () in
    let extension = Filename.extension !filename in
    let is_lam = (extension = ".lam") in
    let is_8pus = (extension = ".8pus") in
    let flag_set = !reduce || !alpha || !typecheck in 

    if (flag_set && not is_lam) || (not flag_set && extension != "" && not is_8pus) then (
        Printf.printf "Invalid file extension\n"
    ) else (
      if !reduce then ()
      else if !alpha then ()
      else if !typecheck then ()
      else Print.print_lam t;
    );
    flush stdout
  with e -> raise e

let _ = run ()
