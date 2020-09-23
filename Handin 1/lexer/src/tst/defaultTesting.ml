(**************************************************************************)
(* AU Compilation.                                                        *)
(**************************************************************************)

(* See comments in runtests.ml *)

open TigerGolden
open TestPhases



let error_code = function
| LEX   -> 10
| PAR   -> 20
| SEM   -> 30
| HOIST -> 32
| LL _  -> 40
| X86 _ -> 50


let string_match r s = 
  try Str.search_forward r s 0|> fun _ -> true  
  with Not_found -> false    

let get_feature_hashtags_for_phase fsetMap phase fname = 
  match assignment_of_phase phase with 
      Some asgmt -> 
         (                 
            try
               let asgmt_map = StringMap.find fname fsetMap in 
                  match AssignmentMap.find_opt asgmt asgmt_map with 
                    Some intset ->
                        let s = IntSet.fold 
                              (fun j s1 -> s1^("#" ^ string_of_assignment asgmt ^ string_of_int j)) intset "" in                         
                        fname ^ " "  ^ s                  
                  
                  | None -> fname 
            with     
              Not_found -> fname 
         )
    | None  -> fname
  
let feature_set_match re fsetMap phase fname =
  match assignment_of_phase phase with 
      Some asgmt -> 
         (                 
            try
               let asgmt_map = StringMap.find fname fsetMap in 
                  match AssignmentMap.find_opt asgmt asgmt_map with 
                    Some intset ->
                        let s = IntSet.fold 
                              (fun j s1 -> s1^("#" ^ string_of_assignment asgmt ^ string_of_int j)) intset "" in 
                        let b = string_match re s in                 
                        (* Printf.printf "%s %s %b\n" fname s b;  *)
                        b                  
                  
                  | None -> false 
            with     
              Not_found -> 
                (Printf.fprintf stdout "No featureset information for file %s\n" fname;
                false)
         )
    | None  -> false 
  

let testCasesFromFiles options phase tester files = 
  let nameFormatter =  get_feature_hashtags_for_phase options.feature_map phase in 
  let filtered_based_on_feature_sets = 
    match options.feature_sets with 
      None -> files 
    | Some re -> List.filter (feature_set_match re options.feature_map phase) files in 
  List.map (testFileWithTester nameFormatter tester) filtered_based_on_feature_sets
  

let defaultTestPos phase = 
  match phase with 
     LEX  | PAR | SEM -> goldenWithExitCode 0 phase
   | HOIST | LL _ | X86 _ -> goldenBackend phase 

let defaultTestNeg phase = goldenWithExitCode @@ error_code @@ phase


let defaultTestsForPhases options phases  = 
  let only_pattern_matcher = 
        match options.use_filter with 
          None -> fun _ -> true 
        | Some r -> string_match r in

  
  let extraMatcher = fun fname -> only_pattern_matcher fname 
                               in 

  let findTigs = Util.findByExtension ~extraMatcher [".tig"] in
  let neg_lex = findTigs "testcases/neg/lex"  in
  let neg_par = findTigs "testcases/neg/par"  in
  let neg_sem = findTigs "testcases/neg/sem" in 
  let pos_batch = findTigs "testcases/pos/batch" in
  let pos_io = findTigs "testcases/pos/interactive"  in

  let posTests = function 
    LEX -> neg_par @ neg_sem @ pos_batch @ pos_io 
  | PAR -> neg_sem @ pos_batch @ pos_io 
  | SEM -> pos_batch @ pos_io 
  | HOIST -> pos_batch @pos_io 
  | LL Batch -> pos_batch 
  | X86 Batch -> pos_batch 
  | _ -> pos_io in 

  let negTests = function 
    LEX -> neg_lex 
  | PAR -> neg_par 
  | SEM -> neg_sem
  | _  -> [] in 


  let defaultPositives phase = 
    testCasesFromFiles options phase (defaultTestPos phase options.overwrite) (posTests phase) in 

  let defaultNegatives phase = 
    testCasesFromFiles options phase ((defaultTestNeg phase) phase options.overwrite)  (negTests phase) in 

  (* create a list of default tests for a phase *)
  let defaultTests  phase = 
  let pos_qualifier = 
      match phase with 
        | HOIST -> "+<exit-code-only>"
        | LL Interactive | X86 Interactive -> "+<compile-only>" 
        | LL Batch | X86 Batch -> "+<exec>"
        | _ -> "+" in 
  let name = phaseName phase in 
  [name ^  pos_qualifier, defaultPositives  phase;
  name ^  "-", defaultNegatives  phase] in 

  phases 
  |> List.map (defaultTests)
  |> List.concat 







     

