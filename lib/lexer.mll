{
  open Parser         
}

let chiffre = ['0'-'9']
let lettre_lc = ['a'-'z']               (* lowercase letter *)
let lettre_uc = ['A'-'Z']               (* uppercase letter *)
let lidentifiant = lettre_lc+ chiffre*  (* lowercase identifier *)
let uidentifiant = lettre_uc+ chiffre*  (* uppercase identifier *)
               
rule token = parse
  | [' ' '\t' '\n'] { token lexbuf } (* ignore space and line breaks *)
  | eof             { EOF } (* end of file *)

  (* Parentheses *)
  | '('             { LPAREN }
  | ')'             { RPAREN }

  (* Keywords for terms *)
  | "fun"           { FUN }
  | ':'             { COLON }
  | "=>"            { DBLARROW }
  | "exf"           { EXF }
  | "fst"           { FST }
  | "snd"           { SND }
  | "ig"            { IG }
  | "id"            { ID }
  | ','             { COMMA }
  | "case"          { CASE }
  | "I"             { LTRUE }

  (* Keywords for types *)
  | "->"            { ARROW }
  | "False"         { FALSE }
  | '~'             { NOT }
  | "True"          { TRUE }
  | "/\\"           { CONJ }
  | "\\/"           { DISJ }
  
  (* Keyword for .lam and .8pus files *)
  | '&'             { AMP }
  | '.'             { DOT }

  
  (* Keywords for special tactics *)
  | "Goal"          { GOAL }
  | "Qed"           { QED }
  | "Show"          { SHOW }
  | "Proof"         { PROOF }

  (* Keywords for proof tactics *)
  | "exact"         { EXACT }
  | "trivial"       { TRIVIAL }
  | "intro"         { INTRO }
  | "intros"        { INTROS }
  | "apply"         { APPLY }
  | "cut"           { CUT }
  | "exfalso"       { EXFALSO }
  | "destruct"      { DESTRUCT }
  | "absurd"        { ABSURD }
  | "admit"         { ADMIT }
  | "split"         { SPLIT }
  | "left"          { LEFT }
  | "right"         { RIGHT }

  | lidentifiant as id { LID id }
  | uidentifiant as id { UID id }
  