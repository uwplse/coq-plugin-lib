(*
 * Utilities for types
 *)

open Environ
open Evd
open Constr

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
