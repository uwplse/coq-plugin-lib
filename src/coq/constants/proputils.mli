(*
 * Utilities for Prop logical types
 *)

open Constr

(* --- Constants --- *)

val logical_or : types
val or_introl : types
val or_intror : types
val logical_and : types
val conj : types

  
(* --- Destruct applications of constants --- *)

(* 
 * or_introl : forall A B : Prop, A -> A \/ B 
 *)
type or_introl_args = {
    a : types;
    b : types;
    ltrm : constr;
  }

(*
 * or_intror : forall A B : Prop, B -> A \/ B 
 *)
type or_intror_args = {
    a : types;
    b : types;
    rtrm : constr;
  }

(*
 * conj : forall A B : Prop, A -> B -> A /\ B 
 *)
type conj_args = {
    a : types;
    b : types;
    ltrm : constr;
    rtrm : constr;
  }
                    
val dest_or_introl : constr -> or_introl_args option
val dest_or_intror : constr -> or_intror_args option
val dest_conj : constr -> conj_args option
