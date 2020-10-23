(**************************************************************************)
(* AU compilation.                                                        *)
(* Skeleton file -- expected to be modified as part of the assignment     *)
(**************************************************************************)

open Tigercommon 
module S = Symbol 
module Ty = Types

type enventry 
  = VarEntry of Types.ty
  | FunEntry of { formals: Types.ty list; result: Types.ty }


(* Should contain base INT and String *)  
let baseTenv = S.empty (* TODO *)

(* Should contain tiger standard library - see Appel p.519 *)
let baseVenv = S.empty(* TODO *)
