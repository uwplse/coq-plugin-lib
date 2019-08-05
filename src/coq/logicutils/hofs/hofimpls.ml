(*
 * Specific implementations of higher-order functions from Hofs
 *)

open Hofs
open Constr
open Debruijn

(*
 * Check recursively whether a term contains another term
 *)
let contains_term c trm =
  exists_subterm equal shift c trm
