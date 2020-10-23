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
  = { venv: E.enventry S.table
    ; tenv: Ty.ty S.table
    ; err :  Err.errenv 
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
    | A.IntExp n -> IntExp n ^! T.INT 
    | A.OpExp {left; oper; right} ->
        let e_left, t_left = e_ty (trexp left) in 
        let e_right, t_right = e_ty (trexp right) in 
        (match t_left, t_right with 
          T.INT, T.INT -> 
              (match oper with
                PlusOp -> OpExp {left = e_left; right = e_right; oper = PlusOp} ^! T.INT
                | MinusOp -> OpExp {left = e_left; right = e_right; oper = MinusOp} ^! T.INT
                | DivideOp -> OpExp {left = e_left; right = e_right; oper = DivideOp} ^! T.INT
                | TimesOp -> OpExp {left = e_left; right = e_right; oper = TimesOp} ^! T.INT
                | ExponentOp -> OpExp {left = e_left; right = e_right; oper = ExponentOp} ^! T.INT
                | EqOp -> OpExp {left = e_left; right = e_right; oper = EqOp} ^! T.INT
                | NeqOp -> OpExp {left = e_left; right = e_right; oper = NeqOp} ^! T.INT
                | LtOp -> OpExp {left = e_left; right = e_right; oper = LtOp} ^! T.INT
                | LeOp -> OpExp {left = e_left; right = e_right; oper = LeOp} ^! T.INT
                | GtOp -> OpExp {left = e_left; right = e_right; oper = GtOp} ^! T.INT
                | GeOp -> OpExp {left = e_left; right = e_right; oper = GeOp} ^! T.INT
                | _ -> raise ThisShouldBeProperErrorMessage
              )
          | T.STRING, T.STRING ->
              (match oper with
              | EqOp -> OpExp {left = e_left; right = e_right; oper = EqOp} ^! T.INT
              | NeqOp -> OpExp {left = e_left; right = e_right; oper = NeqOp} ^! T.INT
              | LtOp -> OpExp {left = e_left; right = e_right; oper = LtOp} ^! T.INT
              | LeOp -> OpExp {left = e_left; right = e_right; oper = LeOp} ^! T.INT
              | GtOp -> OpExp {left = e_left; right = e_right; oper = GtOp} ^! T.INT
              | GeOp -> OpExp {left = e_left; right = e_right; oper = GeOp} ^! T.INT
              | _ -> raise ThisShouldBeProperErrorMessage
              )
          | _ -> raise NotEnoughTime)        
    | _ -> raise NotImplemented
  and trvar (A.Var{var_base;_}) = 
    match var_base with
    | A.SimpleVar _ ->
      raise NotImplemented
    | A.FieldVar (_, _) -> 
      raise NotImplemented
    | A.SubscriptVar (_, _) ->
      raise NotImplemented
  in trexp

and transDecl _ dec = 
  match dec with 
  | A.VarDec _  ->
      raise NotImplemented

  | A.FunctionDec _ ->
      raise NotImplemented
  | A.TypeDec _ ->
      raise NotImplemented
      
let transProg (p: A.exp): exp * Err.errenv = 
  let err =  Err.initial_env in
  
    (transExp ({ venv = E.baseVenv
               ; tenv = E.baseTenv
               ; err = err}
              ) p, err) 
  