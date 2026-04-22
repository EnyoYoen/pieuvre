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

  | "->"            { ARROW }
  | "False"         { FALSE }

  | lidentifiant as id { LID id }
  | uidentifiant as id { UID id }
  