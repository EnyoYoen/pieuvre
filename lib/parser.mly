%{
open Type
open Term
%}

%token EOF
%token LPAREN RPAREN
%token FUN COLON DBLARROW EXF
%token ARROW FALSE NOT
%token <string> LID
%token <string> UID

%start main
%type <Term.lam> main
                                                      
%%

main:
e=term EOF { e }                                                                                

term:
  | FUN LPAREN v=LID COLON t=ltype RPAREN DBLARROW l=term { Abstraction (v, t, l) }
  | l1=term l2=term                                       { Application (l1, l2) }
  | v=LID                                                 { Variable v }
  | EXF LPAREN l=term COLON t=ltype RPAREN                { ExFalso (l, t) }
  | LPAREN l=term RPAREN                                  { l }

ltype:
  | b=UID                     { Base b }
  | t1=ltype ARROW t2=ltype   { Implication (t1, t2) }
  | NOT t=ltype               { Implication (t, False) }
  | FALSE                     { False}
  | LPAREN t=ltype RPAREN     { t }
