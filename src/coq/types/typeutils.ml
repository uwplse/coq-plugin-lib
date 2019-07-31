(*
 * Utilities for types
 *)

open Environ
open Evd
open Constr
open Termdiffutils
open Coqterms

(* --- Type inference --- *)

(* Infer the type of trm in env *)
let infer_type (env : env) (evd : evar_map) (trm : types) : types =
  EConstr.to_constr evd (Typing.unsafe_type_of env evd (EConstr.of_constr trm))
                    
(* Safely infer the WHNF type of a term, updating the evar map *)
let e_infer_type env evm term =
  EConstr.of_constr term |> Typing.e_type_of ~refresh:true env evm |>
  Reductionops.whd_all env !evm |> EConstr.to_constr !evm

(* Safely infer the sort of a type, updating the evar map *)
let e_infer_sort env evm term =
  EConstr.of_constr term |> Typing.e_sort_of env evm |> Sorts.family

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
