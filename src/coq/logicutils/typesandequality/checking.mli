(*
 * Type checking
 *)

open Environ
open Evd
open Constr

(* Check whether a term has a given type *)
val has_type : env -> evar_map -> types -> types -> bool
