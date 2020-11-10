(**************************************************************************)
(* AU Compilation. Assignment submissions must not modify this file       *)
(**************************************************************************)

open Tigercommon
module S = Symbol 
module T = Tabsyn 
module A = Alphaabsyn 
module Ty = Types 
module PrTy = Prtypes

exception AlphaFatal 

let look (e, k) = 
  match S.look (e,k) with Some r -> r | None -> raise AlphaFatal

module RefMap = Map.Make
  (struct type t = Ty.ty option ref
  let compare = 
    let rec compare_aux a b =
      match !a,!b with
    | Some (Ty.NAME (s,r)), Some (Ty.NAME (s',r')) when s = s' ->
      compare_aux r r'
    | Some (Ty.NAME (s,_)), Some (Ty.NAME (s',_)) -> compare s s'
    | Some (Ty.RECORD (_,u)), Some (Ty.RECORD(_,u')) -> compare u u'
    | Some (Ty.ARRAY (_,u)), Some (Ty.ARRAY(_,u')) -> compare u u'
    | _ -> compare a b in
    compare_aux end)

type context = 
  { venv : S.symbol S.table
  ; tenv : S.symbol S.table
  ; fresh_counter : int ref 
  ; mutable refmap : Ty.ty option ref RefMap.t  
  }

let trans_ty (ctxt:context) (ty:Ty.ty): Ty.ty = 
  match ty with 
  | NAME (s, r) -> NAME (look (ctxt.tenv, s),  RefMap.find r ctxt.refmap )
  |  _  -> ty

let gensym {fresh_counter;_} s = 
  let c = !fresh_counter in 
  fresh_counter := c + 1 ;
  S.symbol @@ (S.name s) ^ "$" ^ string_of_int c  

let rec alpha_type (ctxt:context) (ty:Ty.ty) : Ty.ty = 
  match ty with 
  | Ty.RECORD ( fields, uq) -> 
    Ty.RECORD (List.map (fun (s, ty )->                             
        s, alpha_type ctxt ty
      ) fields
              , uq)
  | Ty.ARRAY (ty, uq) -> Ty.ARRAY ( alpha_type ctxt ty,uq)
  | Ty.NAME (s, r) ->
    let r' =
      match RefMap.find_opt r ctxt.refmap with 
        Some r' ->  r' 
      | None -> 
        match r with 
          { contents = Some ty } -> 
          let r' = ref None in 
          ctxt.refmap <- RefMap.add r r' ctxt.refmap;                      
          let t = alpha_type ctxt ty in 
          r' := Some t ;
          r' 
        | _ -> raise AlphaFatal in      
    Ty.NAME (look (ctxt.tenv,s), r')   
  | _ -> ty

let e_ty (A.Exp{ty;_} as e) = (e,ty)
let v_ty (A.Var{ty;_} as v) = (v,ty)

let rec alpha_exp (ctxt:context) (Exp{exp_base;pos;ty} : T.exp) : A.exp =
  let (^!) exp_base ty = A.Exp {exp_base;pos;ty} in
  let alpha = alpha_exp ctxt in  
  match exp_base with 
  | T.VarExp x -> 
    let (v, v_ty) = v_ty @@ alpha_var ctxt x in A.VarExp (v) ^! v_ty 
  | T.NilExp -> A.NilExp ^! Ty.NIL 
  | T.IntExp n -> A.IntExp n ^! Ty.INT 
  | T.StringExp s -> A.StringExp s ^! Ty.STRING
  | T.CallExp { func; args} ->
    let func = look (ctxt.venv, func) in
    let args = List.map alpha args in
    A.CallExp {func; args} ^! trans_ty ctxt ty 
  | T.OpExp { left; oper; right } ->
    let t' = trans_ty ctxt ty  in 
    A.OpExp{left = alpha left; oper; right = alpha right} ^! t'          
  | T.RecordExp { fields } ->
    let fields = List.map (fun (s, e) -> (s, alpha e)) fields in
    A.RecordExp{ fields } ^! trans_ty ctxt ty 
  | T.SeqExp exps ->     
    let exps' = List.map alpha exps in 
    A.SeqExp exps' ^! (match exps' with 
          [] -> Ty.VOID 
        | _ -> List.rev exps'
               |> List.hd |> e_ty |> snd )
  | T.AssignExp{var;exp} ->
    A.AssignExp{var = alpha_var ctxt var;exp = alpha exp} 
    ^! Ty.VOID 
  | T.IfExp {test; thn; els} ->
    A.IfExp { test = alpha test; thn = alpha thn
            ; els = Option.map alpha els} ^! trans_ty ctxt ty
  | T.WhileExp {test; body} ->
    A.WhileExp { test = alpha test; body = alpha body} ^! Ty.VOID 
  | T.ForExp {var; escape; lo; hi; body} ->
    let var' = gensym ctxt var in
    let ctxt' = {ctxt with venv = S.enter (ctxt.venv, var, var')} in
    A.ForExp { var = var'
             ; escape
             ; lo = alpha lo
             ; hi = alpha hi
             ; body = alpha_exp ctxt' body
             } ^! Ty.VOID 
  | T.BreakExp -> A.BreakExp ^! Ty.VOID
  | T.LetExp{decls;body} ->
    let (ctxt',decls_rev) = List.fold_left (
        fun (ctxt,ds) d ->
          let (ctxt',d) = alpha_dec ctxt d in
          (ctxt', d :: ds)
      ) (ctxt,[]) decls in
    let body, body_ty = e_ty @@ alpha_exp ctxt' body in 
    A.LetExp{
      decls = List.rev decls_rev;
      body
    } ^! body_ty 
  | T.ArrayExp{size;init} ->
    A.ArrayExp { size = alpha size
               ; init = alpha init
               } ^! trans_ty ctxt ty  
  | T.ErrorExp -> raise AlphaFatal

and alpha_var (ctxt:context) (Var{var_base;pos;ty}:T.var) : A.var = 
  let (^@) var_base ty = A.Var {var_base;pos;ty} in
  let base = match var_base with 
    | T.SimpleVar s -> A.SimpleVar (look (ctxt.venv, s))
    | T.FieldVar (x, s) -> A.FieldVar (alpha_var ctxt x, s ) 
    | T.SubscriptVar (x, exp) -> 
      A.SubscriptVar ( alpha_var ctxt x, alpha_exp ctxt exp) 
  in base ^@ trans_ty ctxt ty 

and alpha_dec (ctxt:context) (dec:T.decl): (context * A.decl) = 
  match dec with 
    VarDec {name; escape; typ; init; pos} -> 
    let init' = alpha_exp ctxt init in 
    let name' = gensym ctxt name  in 
    let ctxt' = {ctxt with venv = S.enter (ctxt.venv, name, name')} in 
    let typ' = trans_ty ctxt typ in 
    ctxt', A.VarDec { name = name'; escape;typ = typ'; init = init'; pos }  

  | TypeDec decs -> 
    let tenv = List.fold_left           
        ( fun tenv (T.Tdecl {name; _}) -> 
            S.enter (tenv, name, gensym ctxt name))
        ctxt.tenv decs in
    let ctxt' = {ctxt with tenv} in 
    let decs' = List.map 
        (fun (T.Tdecl {name; ty; pos}) 
          -> A.Tdecl {name = look (tenv, name)
                     ; ty = alpha_type ctxt' ty
                     ; pos }) 
        decs in 
    ctxt', A.TypeDec decs'
  | FunctionDec fdecs -> 
    let venv = List.fold_left 
        ( fun venv (T.Fdecl{name; _}) -> 
            S.enter (venv, name , gensym ctxt name))
        ctxt.venv fdecs in 
    let ctxt = {ctxt with venv} in 
    let fdecs = List.map (fun (T.Fdecl {name; args; result; body; pos } ) ->
        let venv', args =
          List.fold_left (fun (venv,ar_) (T.Arg{name; escape; ty; pos}) -> 
              let n' = gensym ctxt name in 
              let ty = alpha_type ctxt ty in 
              let venv' = S.enter (venv, name, n') in 
              (venv', A.Arg { name = n'; escape; ty; pos}:: ar_)
            ) (venv,[]) args in 
        let result = alpha_type ctxt result in 
        let body = alpha_exp {ctxt with venv = venv'} body in 
        let args = List.rev args in         
        A.Fdecl {name = look (venv, name); args; result; body; pos}
      ) fdecs in 
    ctxt, A.FunctionDec fdecs  

let from_string_unchanged ls acc =
  List.fold_left
    (fun t s -> let sym = S.symbol s in S.enter (t, sym,sym )) acc ls 

let from_string_changed ls acc =
  List.fold_left
    (fun t (s,s') ->
       let sym = S.symbol s 
       and sym' = S.symbol s' in 
       S.enter (t, sym,sym' )) acc ls 

let alpha_convert (texp: T.exp) : A.exp = 

  let venv = S.empty |>
             from_string_unchanged 
               [ "print"
               ; "flush"            
               ; "ord"
               ; "chr"
               ; "size"
               ; "substring"
               ; "concat"
               ; "not"] |>
             from_string_changed
               [ ("getchar","getChar")
               ; ("exit", "tigerexit")] in
  let tenv = S.empty |>
             from_string_unchanged ["int" ; "string"] in

  alpha_exp { venv
            ; tenv
            ; fresh_counter = ref 0
            ; refmap = RefMap.empty
            } texp
