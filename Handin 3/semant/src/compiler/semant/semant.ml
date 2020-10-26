(**************************************************************************)
(* AU compilation.                                                        *)
(* Skeleton file -- expected to be modified as part of the assignment     *)
(**************************************************************************)
open Tigercommon

module S = Symbol
module A = Absyn
module E = Semenv
module Err = Errenv
module EFmt = ErrorFormatter
module Ty = Types
module PT = Prtypes 
open Tabsyn

(** Context record contains the environments we use in our translation *)

type context 
  = { venv: E.enventry S.table (* Variable environment, is Gamma in couse document *)
    ; tenv: Ty.ty S.table      (* Type environment, is Delta in course document *)
    ; err :  Err.errenv
    ; breakable : bool
    }
;;

let e_ty (Exp{ty;_} as e) = (e,ty)
let v_ty (Var{ty;_} as v) = (v,ty)

exception NotImplemented
exception NotEnoughTime 
exception ThisShouldBeProperErrorMessage
let actualTy _: T.ty = raise NotImplemented


(* Obs: the infix operators to help with creating expressions. 

   If you want to redefine these to your liking, see 'Prefix and infix symbols' 
   at https://caml.inria.fr/pub/docs/manual-ocaml/lex.html for the rules  *)

let rec transExp (ctxt: context) =  
  let rec trexp (A.Exp{exp_base;pos}) = 
    let (^!) exp_base ty = Exp {exp_base;pos;ty} in
    match exp_base with 
    | A.NilExp -> NilExp ^! T.NIL
    | A.IntExp n -> IntExp n ^! T.INT 
    | A.StringExp s -> StringExp s ^! T.STRING
    | A.OpExp {left; oper; right} ->
        let e_left, t_left = e_ty (trexp left) in 
        let e_right, t_right = e_ty (trexp right) in 
        (match oper with
          | PlusOp | MinusOp | DivideOp | TimesOp | ExponentOp -> 
            (match t_left, t_right with
              | T.INT, T.INT -> OpExp {left = e_left; right = e_right; oper = oper} ^! T.INT
              | _ -> Err.error ctxt.err pos (EFmt.errorArith); ErrorExp ^! T.ERROR
            )
          | LtOp | LeOp | GtOp | GeOp ->
            (match t_left, t_right with
              | T.INT, T.INT | T.STRING, T.STRING -> 
                OpExp {left = e_left; right = e_right; oper = oper} ^! T.INT
              | _ -> Err.error ctxt.err pos (EFmt.errorOtherComparison t_left t_right); ErrorExp ^! T.ERROR
            )
          | EqOp | NeqOp ->
            (match t_left, t_right with
              | T.INT, T.INT | T.STRING, T.STRING | T.RECORD (_, _), T.RECORD (_, _) | T.ARRAY _, T.ARRAY _ ->
                OpExp {left = e_left; right = e_right; oper = oper} ^! T.INT
              | _ -> Err.error ctxt.err pos (EFmt.errorEqNeqComparison t_left t_right); ErrorExp ^! T.ERROR
            )
        )        
    | A.CallExp {func; args} -> raise NotImplemented
    | A.RecordExp {fields} -> raise NotImplemented
    | A.SeqExp expList -> raise NotImplemented
    | A.AssignExp {var; exp} -> raise NotImplemented
    | A.IfExp {test; thn; els} -> 
      let e_test, t_test = e_ty (trexp test) in
      let e_thn, t_thn = e_ty (trexp thn) in 
      (match els with
        | Some e -> 
          let e_els, t_els = e_ty(trexp e) in
          (match t_els with
            | t_thn -> IfExp {test = e_test; thn = e_thn; els = Some e_els} ^! t_els
            | _ -> Err.error ctxt.err pos (EFmt.errorIfBranchesNotSameType t_thn t_els); ErrorExp ^! T.ERROR
          )
        | None ->
          (match t_thn with
            | T.VOID -> IfExp{test = e_test; thn = e_thn; els = None} ^! T.VOID
            | _ -> Err.error ctxt.err pos (EFmt.errorIfThenShouldBeVoid t_thn); ErrorExp ^! T.ERROR
          )
      ) 
    | A.WhileExp {test; body} -> 
      let e_test, t_test = e_ty (trexp test) in
      (match t_test with
        | T.INT ->
          let ctxt = {venv = ctxt.venv; tenv = ctxt.tenv; err = ctxt.err; breakable = true} in
          let e_body, t_body = e_ty(trexp body) in
          (match t_body with
            | T.VOID -> WhileExp {test = e_test; body = e_body} ^! T.VOID
            | _ -> Err.error ctxt.err pos (EFmt.errorWhileShouldBeVoid t_body); ErrorExp ^! T.ERROR
          )
        | _ -> Err.error ctxt.err pos (EFmt.errorIntRequired t_test); ErrorExp ^! T.ERROR
      )
    | A.ForExp {var; escape; lo; hi; body} -> 
      let e_lo, t_lo = e_ty (trexp lo) in
      let e_hi, t_hi = e_ty (trexp hi) in
      (match t_lo with
        | T.INT -> 
          (match t_hi with 
            | T.INT ->  
              let ctxt = {ctxt with venv = S.enter (ctxt.venv, var, E.VarEntry t_lo); breakable = true} in
              let e_body, t_body = e_ty(trexp body) in
              (match t_body with
                | T.VOID -> ForExp {var = var; escape = ref true; lo = e_lo; hi = e_hi; body = e_body} ^! T.VOID
                | _ -> Err.error ctxt.err pos (EFmt.errorForShouldBeVoid t_body); ErrorExp ^! T.ERROR
              )
            | _ -> Err.error ctxt.err pos (EFmt.errorIntRequired t_hi); ErrorExp ^! T.ERROR
          )
        | _ -> Err.error ctxt.err pos (EFmt.errorIntRequired t_lo); ErrorExp ^! T.ERROR
      )
    | A.BreakExp -> 
      (match ctxt.breakable with
        | true -> BreakExp ^! T.VOID
        | false -> Err.error ctxt.err pos (EFmt.errorBreak); ErrorExp ^! T.ERROR
      )
    | A.LetExp {decls; body} ->
      let newDecls = List.map (transDecl ctxt) decls in
      let e_body, t_body = e_ty(transExp (ctxt) body) in
      LetExp{decls = newDecls; body = e_body} ^! t_body
    | A.ArrayExp {size; init} -> raise NotImplemented
    | _ -> raise ThisShouldBeProperErrorMessage
  and trvar (A.Var{var_base;pos}) = 
    let (^@) var_base ty = Var {var_base; pos; ty} in
    match var_base with
    | A.SimpleVar x ->
      let enventry = S.look(ctxt.venv, x) in 
      (match enventry with
        | Some a -> 
          (match a with
            | E.VarEntry t -> SimpleVar x ^@ t
            | E.FunEntry _ -> Err.error ctxt.err pos (EFmt.errorUsingFunctionAsVariable x); SimpleVar x ^@ T.ERROR
          )
        | None -> Err.error ctxt.err pos (EFmt.errorVariableUndefined x); SimpleVar x ^@ T.ERROR
      )
    | A.FieldVar (_, _) -> 
      raise NotImplemented
    | A.SubscriptVar (_, _) ->
      raise NotImplemented
  in trexp

and transDecl ctxt dec = 
  match dec with 
  | A.VarDec {name; escape; typ; init; pos}  ->
      (match typ with 
      | Some t -> raise NotImplemented (* With type annotation *)
      | None -> 
        let e_exp, t_exp = e_ty (transExp(ctxt) init) in
        let ctxt = {ctxt with venv = S.enter (ctxt.venv, name, VarEntry t_exp)} in
        VarDec{name = name; escape = escape; typ = t_exp; init = e_exp; pos = pos}
      )
  | A.FunctionDec _ ->
      raise NotImplemented
  | A.TypeDec _ ->
      raise NotImplemented
      
let transProg (p: A.exp): exp * Err.errenv = 
  let err =  Err.initial_env in
  
    (transExp ({ venv = E.baseVenv
               ; tenv = E.baseTenv
               ; err = err
               ; breakable = false}
              ) p, err) 
  