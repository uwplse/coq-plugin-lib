(*
 * Specific implementations of higher-order functions from Hofs
 * TODO move more of the HOFs stuff into here
 *)

open Hofs
open EConstr
open Debruijn

(*
 * Check recursively whether a term contains another term
 *)
let contains_term sigma c trm =
  exists_subterm (eq_constr sigma) (shift sigma) c trm
