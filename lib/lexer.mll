{
  open Parser         
}

let chiffre = ['0'-'9']
let lettre_lc = ['a'-'z']               (* lettre minuscule *)
let lettre_uc = ['A'-'Z']               (* lettre majuscule *)
let lidentifiant = lettre_lc+ chiffre*  (* identifiant en minuscules *)
let uidentifiant = lettre_uc+ chiffre*  (* identifiant en majuscules *)
               
rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }
  | eof             { EOF }

  | '('             { LPAREN }
  | ')'             { RPAREN }

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

  | "->"            { ARROW }
  | "False"         { FALSE }
  | '~'             { NOT }
  | "True"          { TRUE }
  | "/\\"           { CONJ }
  | "\\/"           { DISJ }
  
  | '&'             { AMP }
  | '.'             { DOT }

  
  | "Goal"          { GOAL }
  | "Qed"           { QED }
  | "Show"          { SHOW }
  | "Proof"         { PROOF }

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

  | lidentifiant as id { LID id }
  | uidentifiant as id { UID id }
  