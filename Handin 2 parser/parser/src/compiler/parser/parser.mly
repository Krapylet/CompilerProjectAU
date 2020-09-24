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

%left PLUS, MINUS, TIMES, DIVIDE, AND, OR
%nonassoc LT, LE, GT, GE, EQ, NEQ
%right CARET, ASSIGN, THEN, ELSE


%start <Tigercommon.Absyn.exp> program  
(* Observe that we need to use fully qualified types for the start symbol *)

%%
(* Top-level *)
program: e = exp EOF { e }


exp:
| e=exp_base  { e ^! $startpos }


var:
| v=var_base { v ^@ $startpos }


(* Expressions *)
exp_base:
| NIL  { NilExp}
| i = INT  { IntExp i }
| s = STRING { StringExp s }
(* Loop Expressions *)
| WHILE e1 = exp DO e2 = exp { WhileExp{test = e1; body = e2} }
| FOR id=ID ASSIGN e1 = exp TO e2 = exp DO e3 = exp { 
                            ForExp{var = (symbol id); escape = ref true; lo = e1; hi = e2; body = e3} 
                            }
(* Var Expressions *)
| v = var { VarExp v } 
| v = var ASSIGN e=exp { AssignExp{var=v; exp=e} }
(* Conditional Expressions *) 
| IF e1=exp THEN e2=exp ELSE e3=exp { IfExp{test=e1; thn=e2; els=Some(e3)} }
| IF e1=exp THEN e2=exp { IfExp{test=e1; thn=e2; els=None} }
(* Break Expression*)
| BREAK { BreakExp }
(* Sequence Expression *)
| LPAREN p = separated_list(SEMICOLON, exp) RPAREN {SeqExp p}
(* Let Expression *)
| LET d = nonempty_list(decl) IN e=separated_list(SEMICOLON, exp) END { LetExp{
                                        decls = d 
                                        ; body = (SeqExp e) ^! $startpos
                                        }
                                    }
(* Call Expression *)
| id=ID LPAREN l=separated_list(COMMA, exp) RPAREN { CallExp{func = symbol id; args = l} }
(* BINOP EXP *)
| e1 = exp CARET e2 = exp { OpExp{left = e1; oper = ExponentOp; right = e2} }
| e1 = exp TIMES e2 = exp { OpExp{left = e1; oper = TimesOp; right = e2} }
| e1 = exp DIVIDE e2 = exp { OpExp{left = e1; oper = DivideOp; right = e2} }
| e1 = exp PLUS e2 = exp { OpExp{left = e1; oper = PlusOp; right = e2} }
| e1 = exp MINUS e2 = exp { OpExp{left = e1; oper = MinusOp; right = e2} }
| e1 = exp EQ e2 = exp { OpExp{left = e1; oper = EqOp; right = e2} }
| e1 = exp NEQ e2 = exp { OpExp{left = e1; oper = NeqOp; right = e2} }
| e1 = exp LT e2 = exp { OpExp{left = e1; oper = LtOp; right = e2} }
| e1 = exp LE e2 = exp { OpExp{left = e1; oper = LeOp; right = e2} }
| e1 = exp GT e2 = exp { OpExp{left = e1; oper = GtOp; right = e2} }
| e1 = exp GE e2 = exp { OpExp{left = e1; oper = GeOp; right = e2} }
| e1 = exp AND e2 = exp { let zero = ((IntExp 0) ^! $startpos) in
                            IfExp{test=e1; thn=e2; els=Some(zero) }
                        }
| e1 = exp OR e2 = exp { let one = ((IntExp 1) ^! $startpos) in
                            IfExp{test=e1; thn=one; els=Some(e2) }
                        }

(* i := i - 1 *)

var_base:
(* Simple var i.e. x or var x *)
| VAR id = ID { SimpleVar(symbol id) }
| id = ID { SimpleVar (symbol id) }
(* Field var i.e. y.x*)
| id1=var DOT id2 = ID { FieldVar(id1, symbol id2) }
(* Subscript var i.e. x[e]*)
| id=var LBRACK e=exp RBRACK { SubscriptVar(id, e) }



decl:
| decs = nonempty_list(fundecldata) {FunctionDec decs}
| VAR n = ID COLON t = ID ASSIGN e = exp  {VarDec {name = (symbol n)
                                                  ; escape = ref true
                                                  ; typ = Some((symbol t), $startpos)
                                                  ; init = e
                                                  ; pos = $startpos
                                                  }
                                          }
| VAR n = ID ASSIGN e = exp {VarDec {name = (symbol n)
                                    ; escape = ref true
                                    ; typ = None
                                    ; init = e
                                    ; pos = $startpos
                                    }
                            }
| decs = nonempty_list(tydecldata) {TypeDec decs}

tydecldata:
| TYPE id = ID EQ t = ty { Tdecl{ name=(symbol id); ty=t; pos=$startpos } }

fundecldata:
| FUNCTION name = ID LPAREN p=separated_list(COMMA, fielddata) RPAREN COLON returnType=ID EQ e=exp{ Fdecl {
                                                                      name=(symbol name)
                                                                    ; params=p
                                                                    ; result=Some(symbol(returnType), $startpos)
                                                                    ; body=e
                                                                    ; pos= $startpos
                                                                    }
                                                                  }
| FUNCTION name = ID LPAREN p=separated_list(COMMA, fielddata) RPAREN EQ e=exp { 
                                                                            Fdecl { name=(symbol name)
                                                                                  ; params=p
                                                                                  ; result=None
                                                                                  ; body=e
                                                                                  ; pos= $startpos
                                                                                 }
                                                                            }
fielddata:
| fieldName = ID COLON fieldType = ID {
                                      Field{ name=(symbol fieldName)
                                            ; escape=ref true
                                            ; typ=(symbol fieldType, $startpos)
                                            ; pos=$startpos
                                            }
                                      }
                                      

ty:
| id = ID { NameTy(symbol id, $startpos) }
| LBRACE tyFields = separated_list(COMMA, fielddata) RBRACE { RecordTy tyFields }
| ARRAY OF id = ID { ArrayTy(symbol id, $startpos) }



