(*
 * Type inference
 *)

open Environ
open Evd
open EConstr
open Declarations

(*
 * Safely infer the WHNF type of a term, updating the evar map
 *)
val infer_type : env -> evar_map -> constr -> evar_map * types

(*
 * Safely infer the sort of a term, updating the evar map
 *)
val infer_sort : env -> evar_map -> constr -> evar_map * Sorts.family

(*
 * Get the type of an inductive type
 *)
val type_of_inductive : env -> int -> mutual_inductive_body -> types
