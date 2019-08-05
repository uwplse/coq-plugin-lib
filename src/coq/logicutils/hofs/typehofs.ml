(*
 * Utilities for types
 *)

open Reducers

(* --- Higher-order functions --- *)

(* Apply on types instead of on terms *)
let on_type f env evd trm = f (reduce_type env evd trm)
