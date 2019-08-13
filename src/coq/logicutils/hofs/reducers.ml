(* Strategies for reducing terms (TODO will be way better with EConstr) *)

open Environ
open Evd
open Constr
open Hofs
open Utilities
open Debruijn
open Idutils
open Contextutils
open Inference
open Stateutils
              
type reducer = env -> evar_map -> types -> evar_map * types
type stateless_reducer = env -> evar_map -> types -> types

(* --- Top-level --- *)

(* Default reducer *)
let reduce_term (env : env) (sigma : evar_map) (trm : types) =
  sigma, EConstr.to_constr
    sigma
    (Reductionops.nf_betaiotazeta env sigma (EConstr.of_constr trm))

(* Delta reduction *)
let delta (env : env) (sigma : evar_map) (trm : types) =
  sigma, EConstr.to_constr
    sigma
    (Reductionops.whd_delta env sigma (EConstr.of_constr trm))

(* Weak head reduction *)
let whd (env : env) (sigma : evar_map) (trm : types) =
  sigma, EConstr.to_constr
    sigma
    (Reductionops.whd_all env sigma (EConstr.of_constr trm))

(* nf_all *)
let reduce_nf (env : env) (sigma : evar_map) (trm : types) =
  sigma, EConstr.to_constr
    sigma
    (Reductionops.nf_all env sigma (EConstr.of_constr trm))

(* --- Combinators and converters --- *)

let reduce_all (r : reducer) env sigma (trms : types list) =
  map_fold_state sigma (r env) trms

let chain_reduce (r1 : reducer) (r2 : reducer) env sigma trm =
  fold_tuple (r2 env) (r1 env sigma trm)

let try_reduce (r : reducer) (env : env) sigma (trm : types) =
  try r env sigma trm with _ -> sigma, trm

(*
 * Reduce the body of a term using the supplied reducer if
 * the predicate p is true on the body. If the term is a function,
 * then this recurses into the body and checks the condition, and so on.
 * It reduces as soon as the condition holds.
 *)
let rec reduce_body_if p (r : reducer) env sigma trm =
  let sigma, p_holds = p env sigma trm in
  if p_holds then
    r env sigma trm
  else
    match kind trm with
    | Lambda (n, t, b) ->
       reduce_body_if p r (push_rel CRD.(LocalAssum(n, t)) env) sigma b
    | _ ->
       failwith "Could not specialize"

(* Reduce the type *)
let reduce_type_using r (env : env) sigma (trm : types) : evar_map * types =
  let sigma, typ = infer_type env sigma trm in
  r env sigma typ

(* Reduce the type with the defualt reducer *)
let reduce_type (env : env) sigma (trm : types) : evar_map * types =
  reduce_type_using reduce_term env sigma trm

(* --- Custom reducers --- *)

(* Don't reduce *)
let do_not_reduce (env : env) sigma (trm : types) =
  sigma, trm

(* Remove all applications of the identity function *)
let remove_identities (env : env) sigma (trm : types) =
  sigma, map_term_if
    (fun _ t -> applies_identity t)
    (fun _ t ->
      match kind t with
      | App (_, args) ->
         Array.get args 1
      | _ ->
         t)
    id
    ()
    trm

(* Remove all applications of the identity function, then default reduce *)
let reduce_remove_identities : reducer =
  chain_reduce remove_identities reduce_term

(* Reduce and also unfold definitions *)
let reduce_unfold (env : env) sigma (trm : types) =
  sigma, EConstr.to_constr
    sigma
    (Reductionops.nf_all env sigma (EConstr.of_constr trm))

(* Reduce and also unfold definitions, but weak head *)
let reduce_unfold_whd (env : env) sigma (trm : types) =
  sigma, EConstr.to_constr
    sigma
    (Reductionops.whd_all env sigma (EConstr.of_constr trm))

(* Weak-head reduce a term if it is a let-in *)
let reduce_whd_if_let_in (env : env) sigma (trm : types) =
  if isLetIn trm then
    sigma, EConstr.to_constr
      sigma
      (Reductionops.whd_betaiotazeta sigma (EConstr.of_constr trm))
  else
    sigma, trm

(*
 * This function removes any terms from the hypothesis of a lambda
 * that are not referenced in the body, so that the term
 * has only hypotheses that are referenced.
 *
 * TODO this could feasibly sometimes pass and just change the term;
 * fix that at some point. Really, no reason to be type-checking,
 * even though this is cute and seems to often work in practice
 *)
let rec remove_unused_hypos (env : env) sigma (trm : types) : evar_map * types =
  match kind trm with
  | Lambda (n, t, b) ->
     let env_b = push_rel CRD.(LocalAssum(n, t)) env in
     let sigma, b' = remove_unused_hypos env_b sigma b in
     (try
        let num_rels = nb_rel env in
        let env_ill = push_rel CRD.(LocalAssum (n, mkRel (num_rels + 1))) env in
        let sigma, _ = infer_type env_ill sigma b' in
        remove_unused_hypos env sigma (unshift b')
      with _ ->
        sigma, mkLambda (n, t, b'))
  | _ ->
     sigma, trm

(*
 * Given a reducer, get a stateless version of the reducer that ignores
 * the resulting evar_map. This can be used when we know the evar_map
 * will not change, for example with the normal reduction functions
 * at the term level. If the evar_map is not identical to the input evar_map,
 * then this fails with an error.
 *)
let reduce_stateless r env sigma trm =
  let sigma', trm' = r env sigma trm in
  if sigma == sigma' then
    trm'
  else
    failwith "cannot call reduce_stateless when the evar_map actually changes"
