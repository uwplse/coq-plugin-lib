(*
 * Specific implementations of higher-order functions from Hofs
 *)

open Constr
open Environ

(*
 * Check recursively whether a term contains another term
 * Use exact equality for comparison
 *)
val contains_term : env -> types -> types -> bool