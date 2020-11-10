open Tigercommon
module A = Absyn

let (^!) exp_base pos = A.Exp{exp_base;pos}
let (^@) var_base pos = A.Var{var_base;pos}

let rec normalize (A.Exp{exp_base;pos}) = 
  let strip (A.Exp{exp_base;_}) = exp_base in
  let norm_2_b (a,b) = (a,normalize b) in
  let e = match exp_base with
  | A.VarExp var -> A.VarExp (normalize_var var)
  | A.CallExp {func;args} ->
    A.CallExp { func
              ; args = List.map normalize args }
  | A.OpExp {left;oper;right} ->
    A.OpExp { left = normalize left
            ; oper
            ; right = normalize right }
  | A.RecordExp {fields;typ} ->
    A.RecordExp { fields = List.map norm_2_b fields
                ; typ }
  | A.SeqExp [e] -> strip @@ normalize e (* This is all we want *)
  | A.SeqExp seq -> A.SeqExp (List.map normalize seq)
  | A.AssignExp {var;exp} ->
    A.AssignExp { var = normalize_var var
                ; exp = normalize exp }
  | A.IfExp {test;thn;els} ->
    A.IfExp { test = normalize test
            ; thn = normalize thn
            ; els = Option.map normalize els }
  | A.WhileExp {test;body} ->
    A.WhileExp { test = normalize test
               ; body = normalize body }
  | A.ForExp {var;escape;lo;hi;body} ->
    A.ForExp { var
             ; escape
             ; lo = normalize lo
             ; hi = normalize hi
             ; body = normalize body }
  | A.LetExp {decls;body} ->
    A.LetExp { decls = List.map normalize_decl decls
             ; body = normalize body }
  | A.ArrayExp {typ;size;init} ->
    A.ArrayExp { typ
               ; size = normalize size
               ; init = normalize init }
  | _ -> exp_base in
  e ^! pos

and normalize_decl decl =
  let norm_fdecl (A.Fdecl {name;params;result;body;pos}) =
    A.Fdecl { name
            ; params
            ; result
            ; body = normalize body
            ; pos } in
  match decl with
  | A.FunctionDec decs ->
    A.FunctionDec (List.map norm_fdecl decs)
  | A.VarDec {name;escape;typ;init;pos} ->
    A.VarDec { name
             ; escape
             ; typ
             ; init = normalize init
             ; pos }
  | A.TypeDec _ -> decl

and normalize_var (A.Var{var_base;pos}) =
  let v = match var_base with
  | A.SimpleVar _ -> var_base
  | A.FieldVar (v, s) -> A.FieldVar (normalize_var v, s)
  | A.SubscriptVar (v, e) -> A.SubscriptVar (normalize_var v, normalize e) in
  v ^@ pos
