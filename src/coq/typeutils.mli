(*
 * Utilities for types
 *)

open Environ
open Evd
open Constr
                                                      
(* --- Higher-order functions --- *)

(*
 * Apply a function on a type instead of on the term
 *)
val on_type : (types -> 'a) -> env -> evar_map -> types -> 'a
