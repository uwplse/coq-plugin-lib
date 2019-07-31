(*
 * Utilities for types
 *)

open Environ
open Evd
open Constr
open Coqterms
open Convertibility
open Inference

(* --- Type checking -- *)
                                                       
(* Check whether a term has a given type *)
let has_type (env : env) (evd : evar_map) (typ : types) (trm : types) : bool =
  try
    let trm_typ = infer_type env evd trm in
    convertible env evd trm_typ typ
  with _ -> false

(* --- Reduction on types --- *)

(* Reduce the type *)
let reduce_type (env : env) evd (trm : types) : types =
  reduce_term env (infer_type env evd trm)
              
(* --- Higher-order functions --- *)

(* Apply on types instead of on terms *)
let on_type f env evd trm = f (reduce_type env evd trm)
