%{
open Type
open Term
open Tactic
%}

%token EOF
%token LPAREN RPAREN
%token FUN COLON DBLARROW EXF
%token ARROW FALSE NOT
%token AMP DOT
%token GOAL QED SHOW PROOF
%token EXACT TRIVIAL INTRO INTROS APPLY CUT EXFALSO DESTRUCT ABSURD ADMIT
%token <string> LID
%token <string> UID

%right ARROW
%nonassoc NOT

%start reduce alpha typecheck proof tactic
%type <Term.lam> reduce
%type <Term.lam * Term.lam> alpha
%type <Term.lam * Type.ty> typecheck
%type <Tactic.tactic list> proof
%type <Tactic.tactic> tactic
                                                      
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
  | NOT t=ltype               { Implication (t, False) }
  | FALSE                     { False}
  | LPAREN t=ltype RPAREN     { t }

tactic:
  | GOAL t=ltype DOT    { Goal t }
  | QED DOT             { Qed }
  | SHOW PROOF DOT      { ShowProof }

  | EXACT h=UID DOT     { Exact h }
  | TRIVIAL DOT         { Trivial }
  | INTRO h=UID DOT     { Intro (Some h) }
  | INTRO DOT           { Intro None }
  | INTROS hl=UID* DOT  { Intros hl }
  | APPLY h=UID DOT     { Apply h }
  | CUT DOT             { Cut }
  | EXFALSO DOT         { ExFalso }
  | DESTRUCT DOT        { Destruct }
  | ABSURD DOT          { Absurd }
  | ADMIT DOT           { Admit }

proof:
  tl=tactic_list EOF  { tl }

tactic_list:
  | t=tactic tl=tactic_list { t :: tl }
  |                         { [] }