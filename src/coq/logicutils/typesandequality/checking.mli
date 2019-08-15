(*
 * Type checking
 *)

open Environ
open Evd
open EConstr

(* Check whether a term has a given type *)
val has_type : env -> evar_map -> types -> types -> evar_map * bool
