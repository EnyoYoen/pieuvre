%{
open Type
open Term
open Tactic
%}

%token EOF
%token LPAREN RPAREN
%token FUN COLON DBLARROW EXF FST SND IG ID COMMA CASE LTRUE
%token ARROW FALSE NOT TRUE CONJ DISJ
%token AMP DOT
%token GOAL QED SHOW PROOF
%token EXACT TRIVIAL INTRO INTROS APPLY CUT EXFALSO DESTRUCT ABSURD ADMIT SPLIT LEFT RIGHT
%token <string> LID
%token <string> UID

%right ARROW
%left DISJ
%left CONJ
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
  | LTRUE                                                 { True }
  | v=LID                                                 { Variable v }
  | EXF LPAREN l=term COLON t=ltype RPAREN                { ExFalso (l, t) }
  | LPAREN l1=term COMMA l2=term RPAREN                   { Uple (l1, l2) }
  | FST LPAREN l=term RPAREN                              { First l }
  | SND LPAREN l=term RPAREN                              { Second l }
  | IG LPAREN l=term COMMA t=ltype RPAREN                 { Left (l, t) }
  | ID LPAREN l=term COMMA t=ltype RPAREN                 { Right (l, t) }
  | CASE LPAREN m=term COMMA n1=term COMMA n2=term RPAREN { Case (m, n1, n2) }
  | LPAREN l=term RPAREN                                  { l }


ltype:
  | b=UID                     { Base b }
  | t1=ltype ARROW t2=ltype   { Implication (t1, t2) }
  | NOT t=ltype               { Implication (t, False) }
  | FALSE                     { False }
  | TRUE                      { True }
  | t1=ltype CONJ t2=ltype    { Conjunction (t1, t2) }
  | t1=ltype DISJ t2=ltype    { Disjunction (t1, t2) }
  | LPAREN t=ltype RPAREN     { t }

tactic:
  | GOAL t=ltype DOT    { Goal t }
  | QED DOT             { Qed }
  | SHOW PROOF DOT      { ShowProof }

  | EXACT h=UID DOT     { Exact h }
  | EXACT l=term DOT    { ExactTerm l }
  | TRIVIAL DOT         { Trivial }
  | INTRO h=UID DOT     { Intro (Some h) }
  | INTRO DOT           { Intro None }
  | INTROS hl=UID* DOT  { Intros hl }
  | APPLY h=UID DOT     { Apply h }
  | CUT t=ltype DOT     { Cut t }
  | EXFALSO DOT         { ExFalso }
  | DESTRUCT h=UID DOT  { Destruct h }
  | ABSURD t=ltype DOT  { Absurd t }
  | ADMIT DOT           { Admit }
  | SPLIT DOT           { Split }
  | LEFT DOT            { Left }
  | RIGHT DOT           { Right }

proof:
  tl=tactic_list EOF  { tl }

tactic_list:
  | t=tactic tl=tactic_list { t :: tl }
  |                         { [] }