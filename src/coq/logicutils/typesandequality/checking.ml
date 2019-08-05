(*
 * Type checking
 *)

open Environ
open Evd
open Constr
open Convertibility
open Inference
                                                       
(* Check whether a term has a given type *)
let has_type (env : env) (evd : evar_map) (typ : types) (trm : types) : bool =
  try
    let trm_typ = infer_type env evd trm in
    convertible env evd trm_typ typ
  with _ -> false

