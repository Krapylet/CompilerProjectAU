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

let standardLibrary = 
  [
  ( S.symbol "print", FunEntry{formals = [Ty.STRING]; result = Ty.VOID} )
  ; ( S.symbol "flush", FunEntry {formals = []; result = Ty.VOID} )
  ; ( S.symbol "getchar", FunEntry {formals = []; result = Ty.STRING} )
  ; ( S.symbol "ord", FunEntry {formals = [Ty.STRING]; result = Ty.INT} )
  ; ( S.symbol "chr", FunEntry {formals = [Ty.INT]; result = Ty.STRING} )
  ; ( S.symbol "size", FunEntry {formals = [Ty.STRING]; result = Ty.INT} )
  ; ( S.symbol "substring", FunEntry {formals = [Ty.STRING; Ty.INT; Ty.INT]; result = Ty.STRING} )
  ; ( S.symbol "concat", FunEntry {formals = [Ty.STRING; Ty.STRING]; result = Ty.STRING} )
  ; ( S.symbol "not", FunEntry {formals = [Ty.INT]; result = Ty.INT} )
  ; ( S.symbol "exit", FunEntry {formals = [Ty.INT]; result = Ty.VOID} )
  ]

let rec addToVenv entries acc = 
  match entries with 
    | (s,v)::body -> 
      let acc = S.enter (acc, s, v) in
      addToVenv body acc
    | [] -> acc

(* Should contain tiger standard library - see Appel p.519 *)
let baseVenv = addToVenv standardLibrary S.empty
      

