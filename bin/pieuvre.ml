open Lib

let reduce = ref false          (* takes as input a lambda expression written in a file passed as an argument and displays the sequence of its reductions *)
let alpha = ref false           (* takes two lambda expressions as input, separated by the & character and written to a file passed as an argument, and returns true or false depending on whether they are alpha-convertible or not *)
let typecheck = ref false       (* takes as input a file containing a lambda expression and a type, separated by a colon (:), and ending with a period (.), and verifies that the typing relationship holds *)
let filename = ref ""           (* name of file to check *)

let parse_args () =
  Arg.parse
    [
      ("-reduce", Arg.Set reduce, "Display the sequence of its reduction");
      ("-alpha", Arg.Set alpha, "Check alpha-convertibility");
      ("-typecheck", Arg.Set typecheck, "Check if the typing relationship hold");
    ]
    (fun s -> filename := s)
    ""

let run () =
  try
    parse_args ();
    let extension = Filename.extension !filename in
    let is_lam = (extension = ".lam") in
    let is_8pus = (extension = ".8pus") in
    let flag_set = !reduce || !alpha || !typecheck in 

    if (flag_set && not is_lam) || (not flag_set && extension != "" && not is_8pus) then (
        Printf.printf "Invalid file extension\n"
    ) else (
      let lexbuf = (
        Lexing.from_channel (
          match !filename with "" -> stdin | s -> open_in s
        )
      ) in
      if !reduce then (
        let t = Parser.reduce Lexer.token lexbuf in
        Print.print_reduction_sequence t
      ) else if !alpha then (
        let (t1, t2) = Parser.alpha Lexer.token lexbuf in
        print_endline (string_of_bool (Term.alpha t1 t2))
      ) else if !typecheck then (
        let (t, ty) = Parser.typecheck Lexer.token lexbuf in
        print_endline (string_of_bool (Infer.typecheck [] t ty))
      )
    );
    flush stdout
  with e -> raise e

let _ = run ()
