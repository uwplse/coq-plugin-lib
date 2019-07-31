(*
 * Utilities for differencing types
 *)

open Constr
open Environ
open Evd
open Typeutils
open Termdiffutils

(* --- Convertibility --- *)

(* Checks whether the types of two terms are convertible *)
let types_convertible env evd trm1 trm2 : bool =
  try
    let typ1 = infer_type env evd trm1 in
    let typ2 = infer_type env evd trm2 in
    convertible env evd typ1 typ2
  with _ -> false
