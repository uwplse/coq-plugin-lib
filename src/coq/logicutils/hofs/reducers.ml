(* Strategies for reducing terms *)

open Environ
open Evd
open Constr
open Hofs
open Utilities
open Debruijn
open Idutils
open Contextutils

type reducer = env -> evar_map -> types -> types

(* --- TODO for refactoring without breaking things --- *)

(*
 * Infer the type of trm in env
 * Note: This does not yet use good evar map hygeine; will fix that
 * during the refactor.
 *)
let infer_type (env : env) (evd : evar_map) (trm : types) : types =
  let jmt = Typeops.infer env trm in
  j_type jmt
               
(* --- End TODO --- *)

(* --- Top-level --- *)

(* Default reducer *)
let reduce_term (env : env) (sigma : evar_map) (trm : types) : types =
  EConstr.to_constr
    sigma
    (Reductionops.nf_betaiotazeta env sigma (EConstr.of_constr trm))

(* Delta reduction *)
let delta (env : env) (sigma : evar_map) (trm : types) =
  EConstr.to_constr
    sigma
    (Reductionops.whd_delta env sigma (EConstr.of_constr trm))

(* Weak head reduction *)
let whd (env : env) (sigma : evar_map) (trm : types) : types =
  EConstr.to_constr
    sigma
    (Reductionops.whd_all env sigma (EConstr.of_constr trm))

(* nf_all *)
let reduce_nf (env : env) (sigma : evar_map) (trm : types) : types =
  EConstr.to_constr
    sigma
    (Reductionops.nf_all env sigma (EConstr.of_constr trm))

(* --- Combinators and converters --- *)

let reduce_all (r : reducer) env evd (trms : types list) : types list =
  List.map (r env evd) trms

let chain_reduce (r1 : reducer) (r2 : reducer) env evd trm : types =
  r2 env evd (r1 env evd trm)

let try_reduce (r : reducer) (env : env) evd (trm : types) : types =
  try r env evd trm with _ -> trm

(*
 * Reduce the body of a term using the supplied reducer if
 * the predicate p is true on the body. If the term is a function,
 * then this recurses into the body and checks the condition, and so on.
 * It reduces as soon as the condition holds.
 *)
let rec reduce_body_if p (r : reducer) env evd trm =
  if p env trm then
    r env evd trm
  else
    match kind trm with
    | Lambda (n, t, b) ->
       reduce_body_if p r (push_rel CRD.(LocalAssum(n, t)) env) evd b
    | _ ->
       failwith "Could not specialize"

(* --- Custom reducers --- *)

(* Don't reduce *)
let do_not_reduce (env : env) (evd : evar_map) (trm : types) : types =
  trm

(* Remove all applications of the identity function *)
let remove_identities (env : env) (evd : evar_map) (trm : types) : types =
  map_term_if
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
let reduce_unfold (env : env) (evd : evar_map) (trm : types) : types =
  EConstr.to_constr
    evd
    (Reductionops.nf_all env evd (EConstr.of_constr trm))

(* Reduce and also unfold definitions, but weak head *)
let reduce_unfold_whd (env : env) (evd : evar_map) (trm : types) : types =
  EConstr.to_constr
    evd
    (Reductionops.whd_all env evd (EConstr.of_constr trm))

(* Weak-head reduce a term if it is a let-in *)
let reduce_whd_if_let_in (env : env) (evd : evar_map) (trm : types) : types  =
  if isLetIn trm then
    EConstr.to_constr
      evd
      (Reductionops.whd_betaiotazeta evd (EConstr.of_constr trm))
  else
    trm

(*
 * This function removes any terms from the hypothesis of a lambda
 * that are not referenced in the body, so that the term
 * has only hypotheses that are referenced.
 *)
let rec remove_unused_hypos (env : env) (evd : evar_map) (trm : types) : types =
  match kind trm with
  | Lambda (n, t, b) ->
     let env_b = push_rel CRD.(LocalAssum(n, t)) env in
     let b' = remove_unused_hypos env_b evd b in
     (try
        let num_rels = nb_rel env in
        let env_ill = push_rel CRD.(LocalAssum (n, mkRel (num_rels + 1))) env in
        let _ = infer_type env_ill evd b' in
        remove_unused_hypos env evd (unshift b')
      with _ ->
        mkLambda (n, t, b'))
  | _ ->
     trm

(* Reduce the type (TODO empty here for rev. compat. for now *)
let reduce_type (env : env) evd (trm : types) : types =
  reduce_term env Evd.empty (Inference.infer_type env evd trm)