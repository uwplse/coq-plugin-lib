(*
 * Type inference
 *)

open Constr
open Declarations
             
(* Safely infer the WHNF type of a term, updating the evar map *)
let infer_type env sigma term =
  let eterm = EConstr.of_constr term in
  let (sigma, typ) = Typing.type_of ~refresh:true env sigma eterm in
  sigma, EConstr.to_constr sigma typ

(* Safely infer the sort of a type, updating the evar map *)
let infer_sort env sigma term =
  let eterm = EConstr.of_constr term in
  let (sigma, sort) = Typing.sort_of env sigma eterm in
  sigma, Sorts.family sort

(* Get the type of an inductive type (TODO do we need evar_map here?) *)
let type_of_inductive env index mutind_body : types =
  let ind_bodies = mutind_body.mind_packets in
  let ind_body = Array.get ind_bodies index in
  let univs = Declareops.inductive_polymorphic_context mutind_body in
  let univ_instance = Univ.make_abstract_instance univs in
  let mutind_spec = (mutind_body, ind_body) in
  Inductive.type_of_inductive env (mutind_spec, univ_instance)
