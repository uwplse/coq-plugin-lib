(*
 * Type inference
 *)

open Environ
open Evd
open Constr
open Declarations

(* Infer the type of trm in env *)
let infer_type (env : env) (evd : evar_map) (trm : types) : types =
  EConstr.to_constr evd (Typing.unsafe_type_of env evd (EConstr.of_constr trm))
                    
(* Safely infer the WHNF type of a term, updating the evar map *)
let e_infer_type env evm term =
  EConstr.of_constr term |> Typing.e_type_of ~refresh:true env evm |>
  Reductionops.whd_all env !evm |> EConstr.to_constr !evm

(* Safely infer the sort of a type, updating the evar map (TODO for these, not into the ref thing) *)
let e_infer_sort env evm term =
  EConstr.of_constr term |> Typing.e_sort_of env evm |> Sorts.family

(* Get the type of an inductive type *)
let type_of_inductive env index mutind_body : types =
  let ind_bodies = mutind_body.mind_packets in
  let ind_body = Array.get ind_bodies index in
  let univs = Declareops.inductive_polymorphic_context mutind_body in
  let univ_instance = Univ.make_abstract_instance univs in
  let mutind_spec = (mutind_body, ind_body) in
  Inductive.type_of_inductive env (mutind_spec, univ_instance)
