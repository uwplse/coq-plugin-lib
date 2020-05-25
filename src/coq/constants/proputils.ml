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

  
(* or_introl : forall A B : Prop, A -> A \/ B *)
type or_introl_args = {
    a : types;
    b : types;
    ltrm : constr;
  }

(* or_intror : forall A B : Prop, B -> A \/ B *)
type or_intror_args = {
    a : types;
    b : types;
    rtrm : constr;
  }

(* conj : forall A B : Prop, A -> B -> A /\ B *)
type conj_args = {
    a : types;
    b : types;
    ltrm : constr;
    rtrm : constr;
  }
                    
let dest_or_introl trm : or_introl_args option =
  match kind trm with
  | App (f, args) ->
     if equal f or_introl && Array.length args == 3 then
       Some { a = args.(0) ; b = args.(1) ; ltrm = args.(2)  }
     else
       None
  | _ -> None

let dest_or_intror trm : or_intror_args option =
  match kind trm with
  | App (f, args) ->
     if equal f or_intror && Array.length args == 3 then
       Some { a = args.(0) ; b = args.(1) ; rtrm = args.(2)  }
     else
       None
  | _ -> None
       
let dest_conj trm : conj_args option =
  match kind trm with
  | App (f, args) ->
     if equal f conj && Array.length args == 4 then
       Some { a = args.(0) ; b = args.(1) ; ltrm = args.(2) ; rtrm = args.(3) }
     else
       None
  | _ -> None
