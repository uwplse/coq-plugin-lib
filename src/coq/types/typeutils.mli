(*
 * Utilities for types
 *)

open Environ
open Evd
open Constr

(* --- Type inference ---*)

(*
 * Type inference
 *
 * Current implementation may cause universe leaks, which will just cause
 * conservative failure of the plugin (TODO)
 *)
val infer_type : env -> evar_map -> types -> types

(* Safely infer the WHNF type of a term, updating the evar map. *)
val e_infer_type : env -> evar_map ref -> constr -> constr

(* Safely infer the sort of a term, updating the evar map. *)
val e_infer_sort : env -> evar_map ref -> constr -> Sorts.family

(* --- Type checking --- *)

(* Check whether a term has a given type *)
val has_type : env -> evar_map -> types -> types -> bool

(* --- Reduction on types --- *)

(* betaiotazeta on types *)
val reduce_type : env -> evar_map -> types -> types
                                                      
(* --- Higher-order functions --- *)

(*
 * Apply a function on a type instead of on the term
 *)
val on_type : (types -> 'a) -> env -> evar_map -> types -> 'a
