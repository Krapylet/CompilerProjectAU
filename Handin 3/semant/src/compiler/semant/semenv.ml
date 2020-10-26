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


(* Should contain base INT and STRING *)
let baseTenv = S.enter (S.enter( S.empty, S.symbol "string", Ty.STRING), S.symbol "int", Ty.INT)

(* Should contain tiger standard library - see Appel p.519 *)
(* S.symbol "print", FunEntry {[Ty.STRING]; Ty.VOID} *)
(* S.symbol "flush", FunEntry {[]; Ty.VOID} *)
(* S.symbol "getchar", FunEntry {[]; Ty.STRING} *)
(* S.symbol "ord", FunEntry {[Ty.STRING]; Ty.INT} *)
(* S.symbol "chr", FunEntry {[Ty.INT]; Ty.STRING} *)
(* S.symbol "size", FunEntry {[Ty.STRING]; Ty.INT} *)
(* S.symbol "substring", FunEntry {[Ty.STRING, Ty.INT, Ty.INT]; Ty.STRING} *)
(* S.symbol "concat", FunEntry {[Ty.STRING, Ty.STRING]; Ty.STRING} *)
(* S.symbol "not", FunEntry {[Ty.INT]; Ty.INT} *)
(* S.symbol "exit", FunEntry {[Ty.INT]; Ty.VOID} *)
let baseVenv = S.empty(* TODO *)
