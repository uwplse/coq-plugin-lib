(*
 * Specific implementations of higher-order functions from Hofs
 *)

open Hofs
open Constr

(*
 * Check recursively whether a term contains another term
 * Use exact equality for comparison
 *)
val contains_term : types -> types -> bool
