(*
 * Type checking
 *)

open Environ
open Evd
open Constr
open Convertibility
open Inference
                                                       
(* Check whether a term has a given type *)
let has_type (env : env) (sigma : evar_map) (typ : types) (trm : types) =
  try
    let sigma, trm_typ = infer_type env sigma trm in
    convertible env sigma trm_typ typ
  with _ ->
    sigma, false

