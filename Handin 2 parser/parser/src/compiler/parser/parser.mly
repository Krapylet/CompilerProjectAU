(**************************************************************************)
(* AU compilation.                                                        *)
(* Skeleton file -- expected to be modified as part of the assignment     *)
(* Do not distribute                                                      *)
(**************************************************************************)

%{
  open Tigercommon.Absyn   
  open ParserAux 
  open Tigercommon.Symbol
%}

%token EOF
%token <string> ID
%token <int> INT 
%token <string> STRING 
%token COMMA COLON SEMICOLON 
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE 
%token DOT PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE 
%token AND OR ASSIGN ARRAY IF THEN ELSE WHILE FOR TO DO
%token LET IN END OF BREAK NIL FUNCTION VAR TYPE CARET 

%left PLUS, MINUS, TIMES, DIVIDE
%nonassoc LT, LE, GT, GE, EQ, NEQ
%right CARET


%start <Tigercommon.Absyn.exp> program  
(* Observe that we need to use fully qualified types for the start symbol *)

%%
(* Expressions *)
exp_base:
| NIL  { NilExp}
| i = INT  { IntExp i }
| s = STRING { StringExp s }
(* BINOP EXP *)
| e1 = exp PLUS e2 = exp { OpExp{left = e1; oper = PlusOp; right = e2} }
| e1 = exp MINUS e2 = exp { OpExp{left = e1; oper = MinusOp; right = e2} }
| e1 = exp TIMES e2 = exp { OpExp{left = e1; oper = TimesOp; right = e2} }
| e1 = exp DIVIDE e2 = exp { OpExp{left = e1; oper = DivideOp; right = e2} }
| e1 = exp EQ e2 = exp { OpExp{left = e1; oper = EqOp; right = e2} }
| e1 = exp NEQ e2 = exp { OpExp{left = e1; oper = NeqOp; right = e2} }
| e1 = exp LT e2 = exp { OpExp{left = e1; oper = LtOp; right = e2} }
| e1 = exp LE e2 = exp { OpExp{left = e1; oper = LeOp; right = e2} }
| e1 = exp GT e2 = exp { OpExp{left = e1; oper = GtOp; right = e2} }
| e1 = exp GE e2 = exp { OpExp{left = e1; oper = GeOp; right = e2} }
| e1 = exp CARET e2 = exp { OpExp{left = e1; oper = ExponentOp; right = e2} }
(* AssignEX *)
| WHILE LPAREN e1 = exp RPAREN LBRACE e2 = exp RBRACE {WhileExp{test = e1; body = e2} } 
| IF e1 = exp THEN e2 = exp {IfExp{test = e1; thn = e2} }
| IF e1 = exp THEN e2 = exp ELSE e3 = exp {IfExp{test = e1; thn = e2; els = e3} }

(*| s = symbol LPAREN list = exp* RPAREN { CallExp{ func = s; args = list } }*)

| id = VAR { VarExp{var = id} }
| id = VAR ASSIGN e1 = exp { AssignExp{ var = VarExp{var = id} exp = e1} }
| BREAK { BreakExp }


(* Top-level *)
program: e = exp EOF { e }


exp:
| e=exp_base  { e ^! $startpos }


var:
| v=var_base  { v ^! $startpos}
