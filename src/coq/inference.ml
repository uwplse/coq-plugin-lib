(*
 * Type inference
 *)

open Environ
open Evd
open Constr

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
