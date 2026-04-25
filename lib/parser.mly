%{
open Type
open Term
%}

%token EOF
%token LPAREN RPAREN
%token FUN COLON DBLARROW EXF
%token ARROW FALSE NOT
%token AMP DOT
%token <string> LID
%token <string> UID

%right ARROW
%nonassoc NOT

%start reduce alpha typecheck
%type <Term.lam> reduce
%type <Term.lam * Term.lam> alpha
%type <Term.lam * Type.ty> typecheck
                                                      
%%                                                                            

reduce:
  e=term DOT EOF { e }

alpha:
  l1=term AMP l2=term DOT EOF { (l1, l2) }

typecheck:
  l=term COLON t=ltype DOT EOF { (l, t) }

term:
  | FUN LPAREN v=LID COLON t=ltype RPAREN DBLARROW l=term { Abstraction (v, t, l) }
  | l=application                                         { l }

application:
  | l=application a=atom  { Application (l, a) }
  | a=atom                { a }

atom:
  | v=LID                                  { Variable v }
  | EXF LPAREN l=term COLON t=ltype RPAREN { ExFalso (l, t) }
  | LPAREN l=term RPAREN                   { l }

ltype:
  | b=UID                     { Base b }
  | t1=ltype ARROW t2=ltype   { Implication (t1, t2) }
  | NOT t=ltype %prec NOT     { Implication (t, False) }
  | FALSE                     { False}
  | LPAREN t=ltype RPAREN     { t }
