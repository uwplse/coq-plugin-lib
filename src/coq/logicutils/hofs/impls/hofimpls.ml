(*
 * Specific implementations of higher-order functions from Hofs
 * TODO move more of the HOFs stuff into here
 *)

open Hofs
open Constr
open Debruijn

(*
 * Check recursively whether a term contains another term
 *)
let contains_term c trm =
  exists_subterm equal shift c trm
