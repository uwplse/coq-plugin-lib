(*
 * Utilities for Prop oriented types and tactics
 *)

open Constr
open Names
open Apputils

let coq_init_logic =
  ModPath.MPfile
    (DirPath.make (List.map Id.of_string ["Logic"; "Init"; "Coq"]))

(* --- Constants --- *)

(* Logical or *)
let logical_or : types =
  mkInd (MutInd.make1 (KerName.make2 coq_init_logic (Label.make "or")), 0)
  
(* left constructor of \/ *)
let or_introl : types =
  mkConstruct (fst (destInd logical_or), 1)

(* right constructor of \/ *)
let or_intror : types = 
  mkConstruct (fst (destInd logical_or), 2)


(* Logical and *)
let logical_and : types =
  mkInd (MutInd.make1 (KerName.make2 coq_init_logic (Label.make "and")), 0)
  
(* constructor of /\ *)
let conj : types =
  mkConstruct (fst (destInd logical_and), 1)
