(*
 * Utilities for types
 *)

open Reducers

(* --- Higher-order functions --- *)
       
(* Apply on types instead of on terms; use the supplied reducer to reduce *)
let on_red_type r f env sigma trm =
  let sigma, typ = reduce_type_using r env sigma trm in
  f env sigma typ

(* Instantiations of on_type_red *)
let on_red_type_default f env sigma trm =
  on_red_type reduce_term f env sigma trm

let on_type f env sigma trm =
  on_red_type do_not_reduce f env sigma trm
