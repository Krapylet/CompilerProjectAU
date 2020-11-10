(**************************************************************************)
(* AU Compilation. Assignment submissions must not modify this file       *)
(**************************************************************************)

open Tigercommon.Ll

(* a record for representing the construction of the CFG *)
type cfg_builder = 
  { bb_rev     : (lbl * block) list  
  (* reversed list of all but the first basic blocks *)
  ; insn_rev   : (uid option * insn) list   
  (* reversed list of the instructions that have not yet been 
     allocated a basic block *)
  ; bb_first   : block option  (* first basic block, if any*)
  ; lbl_current: lbl option    (* label of the current basic block*)
  ; allocas_rev: (uid * ty) list      (* reserved alloca's *)
  }



(* this exception is thrown if the module is used incorrectly *)
exception CFGConstructionError of string

(* this exception is thrown if we terminate an unlabelled block *)
exception UnlabelledBlock

(* --- Aauxiliary functions for accessing and manipulating the CFG -- *)

(* add a reserved alloca to the first basic block *)
let add_alloca (uid, ty) (c:cfg_builder)  =
  {c with allocas_rev = (uid, ty)::(c.allocas_rev) }

(* add an instruction to the current list of instructions *)	
let add_insn (insn_: uid option * insn) (c:cfg_builder)  =
  { c with insn_rev = insn_::(c.insn_rev)}

(* terminate the current block *) 
let term_block (tr:terminator) (cfg_:cfg_builder)  =
  let (bb:block) = { insns = List.rev (cfg_.insn_rev)
                   ; terminator = tr }
  in 
  match cfg_.bb_first with 
    None -> { cfg_ with insn_rev = []; bb_first = Some bb; lbl_current = None }	  
  | Some _ -> begin 
      match cfg_.lbl_current with 
        Some lb -> 
        { cfg_ with bb_rev = (lb, bb):: cfg_.bb_rev; insn_rev = []; lbl_current = None }
      | _ -> raise UnlabelledBlock
    end

(* set label for the current block *)
let set_label (lbl: lbl) (c:cfg_builder)  =
  { c with lbl_current = Some lbl }


(* empty CFG environment *)      
let empty_cfg_builder =
  {
    bb_rev      = []
  ; insn_rev    = []
  ; bb_first    = None
  ; lbl_current = None
  ; allocas_rev = []
  }

(* get a CFG from the builder *)      
let get_cfg (cfg_builder:cfg_builder): cfg =
  match cfg_builder.bb_first with 
    Some ({insns; terminator} )   ->
    let allocas = List.map (fun (uid, ty) ->
        (Some uid, Alloca ty))
        (cfg_builder.allocas_rev) in 
    let b' = { insns = allocas @ insns; terminator}
    in (b' , List.rev (cfg_builder.bb_rev ))

  | _ -> raise (CFGConstructionError "basic block is not available")


(* --- buildlets ---------------------------------------------- *)  

type buildlet = cfg_builder -> cfg_builder

let id_buildlet = fun st -> st  (* identity function *)

let seq_buildlets (bs:buildlet list) : buildlet= 
  fun cg -> List.fold_left (fun cg' b -> b cg') cg bs

  