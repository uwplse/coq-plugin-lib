(*
 * Specific implementations of higher-order functions from Hofs
 *)

open EConstr
open Evd

(*
 * Check recursively whether a term contains another term
 * Use exact equality for comparison
 *)
val contains_term : evar_map -> types -> types -> bool
