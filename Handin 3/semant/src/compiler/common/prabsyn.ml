(**************************************************************************)
(* AU Compilation. Assignment submissions must not modify this file       *)
(**************************************************************************)

(** AST pretty printer implementation *)

module S = Symbol

open Absyn 
open Ppcommon


let rec indent i = match i with 
      | 0 -> "" 
      | _ -> indent (i - 1) ^ (if i mod 2 = 0 then "| " else "  ")

let rec dolist d f ls = match ls with 
  |  [a] -> concat ["\n"; f (a, d + 1)]
  |  a::r -> concat ["\n"; f (a, d + 1); ","; dolist d f r]
  |  []  -> ""
      

(* observe that merlin does not handle this function definition properly; aa; 2019-08-07; *)  
let field (Field { name; escape; typ; _}, d) = 
    concat [ indent d; "("; string_of_symbol name; ","
                     ; string_of_bool !escape; ","; string_of_symbol @@ fst typ; ")"]  


let as_string e0 = 
  let rec exp (Exp{exp_base;_}, d) = match exp_base with 
    | VarExp v -> concat [ indent d; "VarExp(\n"; var (v, d+1); ")" ]     
    | IntExp i -> concat [indent d; "IntExp("; string_of_int i; ")"]
    | NilExp -> concat [indent d; "NilExp"]    
    | StringExp s -> concat [indent d; "StringExp(\""; String.escaped s ; "\")"]
    | CallExp { func; args; _ } -> 
        concat [ indent d
               ; "CallExp("
               ; string_of_symbol func
               ; ",["
               ; dolist d exp args
               ; "])"
               ]                
    | OpExp {left; oper; right; _} -> 
        concat [ indent d; "OpExp("; opname oper; ",\n"; exp (left, d+1); ",\n"; exp (right, d + 1); ")"]
    | RecordExp { fields; typ; _ } -> 
       let f ((name, e), d) = 
        concat [indent d; "("; string_of_symbol name; ",\n"; exp (e, d + 1); ")"]
       in concat [ indent d; "RecordExp("; string_of_symbol typ; ",["; dolist d f fields; "])"] 
    | SeqExp l -> 
        concat [indent d; "SeqExp["; dolist d exp l; "]"]
    | AssignExp { var = v; exp = e; _} -> 
       concat [ indent d ; "AssignExp(\n"; var (v, d + 1); ",\n"; exp (e, d + 1 ); ")"]
    | IfExp { test; thn; els; _ }    -> 
       concat [ indent d; "IfExp(\n"; exp (test, d + 1); ",\n"; exp (thn, d + 1); 
                (match els with None -> "" | Some e -> ",\n" ^ exp (e, d + 1)) ; ")"]
    | WhileExp {test; body; _} -> 
       concat [ indent d ; "WhileExp(\n"; exp (test, d + 1); ",\n"; exp (body, d + 1); ")"] 
    | ForExp { var = v; escape = b; lo; hi; body; _} -> 
       concat [ indent d
              ; "ForExp("
              ; string_of_symbol v
              ; ","; string_of_bool (!b)
              ; ",\n"; exp (lo, d + 1)
              ; ",\n"; exp (hi, d + 1)
              ; ",\n"; exp (body, d + 1)
              ; ")"]       
    | BreakExp -> indent d ^ "BreakExp"
    | LetExp {decls; body; _} -> 
        concat [indent d ; "LetExp(["; dolist d dec decls; "],\n"; exp (body, d + 1); ")"] 
    | ArrayExp {typ; size; init; _} -> 
        concat [ indent d; "ArrayExp("; string_of_symbol typ; ",\n"; exp (size, d + 1); ",\n"; exp (init, d + 1); ")"]


  and dec (theDec,d) = match theDec with 
    | FunctionDec l -> 
       
       let f ( Fdecl {name; params; result; body; _}, d ) = 
         concat [ indent d; "("; string_of_symbol name; ",["; dolist d field params; "],\n"
               ; indent (d + 1)
               ; (match result with | None -> "NONE" | Some (s,_) -> "SOME(" ^ string_of_symbol s ^ ")" )
               ; ",\n"
               ; exp (body, d + 1)
               ; ")"] in 
       concat [indent d; "FunctionDec["; dolist d f l; "]"]
    | VarDec { name; escape; typ; init; _ } -> 
        concat [ indent d; "VarDec("; string_of_symbol name; ","; string_of_bool !escape; "," 
              ; (match typ with | None -> "NONE" | Some (s,_) -> "SOME(" ^ string_of_symbol s ^ ")" )
              ; ",\n"
              ; exp (init, d + 1)
              ; ")"]                   
        
    | TypeDec l -> 
         let tdec ( Tdecl {name; ty=t; _}, d ) = 
           concat [ indent d; "("; string_of_symbol name; ",\n"; ty (t, d + 1); ")"] in  
         concat [indent d; "TypeDec["; dolist d tdec l; "]"]
  and ty (theType_, d) = match theType_ with 
    | NameTy (s, _) -> concat [indent d; "NameTy("; string_of_symbol s; ")"]
    | RecordTy l -> concat [indent d; "RecordTy["; dolist d field l; "]"]
    | ArrayTy (s, _) -> concat [indent d; "ArrayTy("; string_of_symbol s; ")"]


  

  and var (Var{var_base;_}, d) = match var_base with
    | SimpleVar s -> 
        concat [indent d; "SimpleVar("; string_of_symbol s; ")"]
    | FieldVar (v,s) -> 
        concat [indent d; "FieldVar(\n" ; var (v, d + 1 )
               ; ",\n"; indent (d + 1); string_of_symbol s; ")"]
    | SubscriptVar (v, e) -> 
        concat [indent d; "SubscriptVar(\n"; var (v, d + 1)
               ; ",\n"; exp (e, d + 1); ")"]
  in exp (e0, 0)


let string_of_exp = as_string  
let print_exp out e = Format.fprintf out "%s\n" (as_string e)
