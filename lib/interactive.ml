open Print
open Process

let run_interactive (lexbuf : Lexing.lexbuf) : unit =
  Printf.printf "Enter tactics ending with a dot. Type 'Qed.' to finish.\n\n%!";

  let rec repl (session : interactive_session) =
    Printf.printf "> %!";
    try
      let tac = Parser.tactic Lexer.token lexbuf in
      match process_interactive_tactic session tac with
      | StepContinue (session', None) ->
        repl session'
      | StepContinue (session', Some sgs) ->
        if sgs = [] then
          Printf.printf "No more goals.\n%!"
        else
          print_subgoal sgs;
        repl session'
      | StepFinished term ->
        Printf.printf "Proof successful! Term:\n%!";
        print_lam term;
        print_newline ()
      | StepError msg ->
        Printf.printf "%s\n%!" msg;
        repl session
    with
    | Parser.Error ->
      Printf.printf "Parse error.\n%!";
      repl session
    | End_of_file -> ()
  in
  repl (None, [], [], None)
