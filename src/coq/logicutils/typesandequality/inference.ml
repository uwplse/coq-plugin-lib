(*
 * Type inference
 *)

open Environ
open Evd
open Constr
open Declarations
             
(* Safely infer the WHNF type of a term, updating the evar map *)
let infer_type env sigma term =
  let sigma_ref = ref sigma in
  let eterm = EConstr.of_constr term in
  let typ = Typing.e_type_of ~refresh:true env sigma_ref eterm in
  let sigma = ! sigma_ref in
  sigma, EConstr.to_constr sigma typ

(* Safely infer the sort of a type, updating the evar map *)
let infer_sort env sigma term =
  let sigma_ref = ref sigma in
  let eterm = EConstr.of_constr term in
  let sort = Typing.e_sort_of env sigma_ref eterm in
  let sigma = ! sigma_ref in
  sigma, Sorts.family sort

(* Get the type of an inductive type (TODO do we need evar_map here?) *)
let type_of_inductive env index mutind_body : types =
  let ind_bodies = mutind_body.mind_packets in
  let ind_body = Array.get ind_bodies index in
  let univs = Declareops.inductive_polymorphic_context mutind_body in
  let univ_instance = Univ.make_abstract_instance univs in
  let mutind_spec = (mutind_body, ind_body) in
  Inductive.type_of_inductive env (mutind_spec, univ_instance)
