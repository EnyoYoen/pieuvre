open Print
open Process

(* Recursive read–eval–print loop for the interactive mode *)
let rec repl (lexbuf : Lexing.lexbuf) (session : interactive_session) =
  Printf.printf "> %!";
  try
    (* We expect a tactic after the '>' *)
    let tac = Parser.tactic Lexer.token lexbuf in
    match process_interactive_tactic session tac with
    | StepContinue (session', None) ->
        repl lexbuf session' (* No active proof *)
    | StepContinue (session', Some sgs) ->
        if sgs = [] then Printf.printf "No more goals.\n%!"
        else print_subgoal sgs;
        repl lexbuf session'
    | StepFinished term ->
        Printf.printf "Proof successful! Term:\n%!";
        print_lam term;
        print_newline ()
    | StepError msg ->
        (* Instead of exiting when there's an error, we print it and go on *)
        Printf.printf "%s\n%!" msg;
        repl lexbuf session
  with
  | Parser.Error ->
      Printf.printf "Parse error.\n%!";
      repl lexbuf session
  | End_of_file -> ()

(* Start the REPL *)
let run_interactive (lexbuf : Lexing.lexbuf) =
  Printf.printf "Enter tactics ending with a dot. Type 'Qed.' to finish.\n\n%!";
  repl lexbuf (None, [], [], None)
