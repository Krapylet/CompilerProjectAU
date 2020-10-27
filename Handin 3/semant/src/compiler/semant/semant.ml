(**************************************************************************)
(* AU compilation.                                                        *)
(* Skeleton file -- expected to be modified as part of the assignment     *)
(**** **********************************************************************)
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
let actualTy _: T.ty = raise NotImplemented (* Should look through NAME types to find actual types*)


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
    | A.VarExp v -> 
      let var, t_var = v_ty(trvar v) in
      VarExp var ^! t_var
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
    | A.CallExp {func; args} -> 
      let enventry = S.look(ctxt.venv, func) in
      (match enventry with
        | Some a ->
          (match a with 
            | E.VarEntry _ -> Err.error ctxt.err pos (EFmt.errorUsingVariableAsFunction func); ErrorExp ^! T.ERROR
            | E.FunEntry {formals; result} ->
              let formalLen = List.length formals in
              let argLen = List.length args in
              let isCorrectNumOfArg = compare formalLen argLen in
              (match isCorrectNumOfArg with
                | 0 ->  
                  let rec iterate_through_args arg_list formal_list acc =
                    (match arg_list, formal_list with
                      | a_head::a_body, f_head::f_body -> 
                        let e_head, t_head = e_ty (trexp a_head) in
                        let isSameType = compare t_head f_head in
                        (match isSameType with
                          | 0 ->
                            let acc = acc @ [e_head] in
                            iterate_through_args a_body f_body acc
                          | _ -> 
                            let acc = acc @ [ErrorExp ^! T.ERROR] in
                            Err.error ctxt.err pos (EFmt.errorWrongArgument f_head t_head); 
                            iterate_through_args a_body f_body acc
                        )
                      | [], [] -> acc
                    ) in
                  let new_args = iterate_through_args args formals [] in
                  CallExp {func = func; args = new_args} ^! result
                | _ -> Err.error ctxt.err pos (EFmt.errorFunctionArguments func); ErrorExp ^! T.ERROR
              )
          )
        | None -> Err.error ctxt.err pos (EFmt.errorFunctionUndefined func); ErrorExp ^! T.ERROR
      )
    | A.RecordExp {fields} -> raise NotImplemented
    | A.SeqExp expList ->
      let rec iterate_through_exps exp_list acc =
      (match exp_list with
        | head::body ->
          let e_head, t_head = e_ty (trexp head) in
          let acc = acc @ [e_head] in
          let n = List.length body in
          (match n with
          | 0 -> SeqExp acc ^! t_head
          | _ ->
            (match t_head with
              | T.NIL -> Err.error ctxt.err pos (EFmt.errorInferNilType); ErrorExp ^! T.ERROR
              | _ -> iterate_through_exps body acc
            )
          )
        | [] -> SeqExp [] ^! T.VOID
      ) in
      iterate_through_exps expList []
    | A.AssignExp {var; exp} -> 
        let v_var, t_var = v_ty(trvar var) in
        let e_exp, t_exp = e_ty(trexp exp) in
        let isSameType = compare t_var t_exp in
        (match isSameType with
          | 0 -> AssignExp{var = v_var; exp = e_exp} ^! T.VOID
          | _ -> Err.error ctxt.err pos (EFmt.errorAssignWrongType t_exp t_var); ErrorExp ^! T.ERROR
        )
    | A.IfExp {test; thn; els} -> 
      let e_test, t_test = e_ty (trexp test) in
      let e_thn, t_thn = e_ty (trexp thn) in 
      (match els with
        | Some e -> 
          let e_els, t_els = e_ty(trexp e) in
          let t = compare t_thn t_els in
          (match t with
            | 0 -> IfExp {test = e_test; thn = e_thn; els = Some e_els} ^! t_els
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
      let new_decls, ctxt = iterate_through_decls decls [] ctxt in
      let e_body, t_body = e_ty(transExp (ctxt) body) in
      LetExp{decls = new_decls; body = e_body} ^! t_body
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
    | A.FieldVar (v, s) -> 
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
        let new_venv = S.enter (ctxt.venv, name, VarEntry t_exp) in 
        let new_ctxt = {ctxt with venv = new_venv} in
        (VarDec{name = name; escape = escape; typ = t_exp; init = e_exp; pos = pos}, new_ctxt)
      )
  | A.FunctionDec funDeclDataList ->(* Should return fundecldata list *)
    let get_symbol (s,_) = s in (* Helper function *)
    let rec iterate_through_funDecls decls acc ctxt =
      (match decls with
        | head::d_body -> 
          (match head with
            | A.Fdecl{name; params; result; body; pos} ->
              let rec iterate_through_args args t_acc arg_acc = (*Returns a list of types and a list of Args*)
                (match args with
                  | a_head::a_body ->
                    (match a_head with
                      | A.Field{name; escape; typ; pos} ->
                          let arg_typ = S.look (ctxt.tenv, get_symbol typ) in
                          (match arg_typ with
                            | Some t ->
                              let t_acc = t_acc @ [t] in
                              let arg_acc = arg_acc @ [Arg{name = name; escape = escape; ty = t; pos = pos}] in
                              iterate_through_args a_body t_acc arg_acc
                            | None -> raise ThisShouldBeProperErrorMessage
                          )
                      | _ -> raise ThisShouldBeProperErrorMessage
                    )
                  | [] -> (t_acc, arg_acc)
                ) in
              let doesFunExist = S.look (ctxt.venv, name) in
              (match doesFunExist with
                | Some f -> 
                  Err.error ctxt.err pos (EFmt.errorDuplicate name); 
                  iterate_through_funDecls d_body acc ctxt
                | None -> 
                  let typ_list, arg_list = iterate_through_args params [] [] in
                  let get_result_type res =
                    (match res with
                      | Some a ->
                        let res_type = S.look (ctxt.tenv, get_symbol a) in
                        (match res_type with
                          | Some r -> r
                          | None -> raise ThisShouldBeProperErrorMessage
                        )
                      | None -> T.VOID
                    ) in
                  let res_type = get_result_type result in
                  let new_venv1 = S.enter (ctxt.venv, name, E.FunEntry{formals = typ_list; result = res_type}) in
                  let new_ctxt1 = {ctxt with venv = new_venv1} in
                  let rec bind_args args venv =
                    (match args with
                      | head::body -> 
                        (match head with
                          | Arg {name; escape; ty; pos} ->
                            let new_venv = S.enter (venv, name, E.VarEntry ty) in
                            bind_args body new_venv
                          | _ -> raise ThisShouldBeProperErrorMessage
                        )
                      | [] -> venv
                    ) in
                  let new_venv2 = bind_args arg_list new_venv1 in
                  let new_ctxt2 = {new_ctxt1 with venv = new_venv2; breakable = false} in
                  let e_body, t_body = e_ty(transExp(new_ctxt2) body) in
                  let isSameType = compare t_body res_type in
                  (match isSameType with
                    | 0 -> 
                      let fun_decl = Fdecl {name = name; args = arg_list; result = res_type; body = e_body; pos = pos} in
                      let acc = acc @ [fun_decl] in
                      iterate_through_funDecls d_body acc new_ctxt1
                    | _ -> 
                      Err.error ctxt.err pos (EFmt.errorFunctionReturn t_body res_type); 
                      iterate_through_funDecls d_body acc new_ctxt1
                  )
              )
            | _ -> raise ThisShouldBeProperErrorMessage
          )
        | [] -> (FunctionDec acc, ctxt)
      ) in
    iterate_through_funDecls funDeclDataList [] ctxt
  | A.TypeDec typeDeclList ->
    let iterate_through_typeDecls decl_list acc ctxt =
      (match decl_list with
        | head::body ->
          (match head with
            | A.Tdecl {name; ty; pos} ->
              (match ty with
                | A.NameTy (symbol, pos) -> raise NotImplemented
                | A.RecordTy fieldDataList -> raise NotImplemented
                | A.ArrayTy (symbol, pos) -> raise NotImplemented
              )
            | _ -> raise ThisShouldBeProperErrorMessage
          )
        | [] -> 
          (* Check for cycles in updated tenv *)
          (acc, ctxt)
    ) in
    let declList, new_ctxt = iterate_through_typeDecls typeDeclList [] ctxt in
    (TypeDec declList, new_ctxt)

(* Helper function *)
(* Takes a list of A.decl, an accumulator and a context*)
(* Returns a list of decl and a updated context *)
and iterate_through_decls decls acc ctxt = 
  (match decls with
    | head::body -> 
      let new_decl, new_ctxt = transDecl ctxt head in
      let acc = acc @ [new_decl] in
      iterate_through_decls body acc new_ctxt
    | [] -> acc, ctxt
  )


  let transProg (p: A.exp): exp * Err.errenv = 
    let err =  Err.initial_env in
  
    (transExp ({ venv = E.baseVenv
               ; tenv = E.baseTenv
               ; err = err
               ; breakable = false}
              ) p, err) 
  