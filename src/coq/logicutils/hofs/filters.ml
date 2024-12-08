(* Filters *)

open Constr
open Environ
open Debruijn
open Evd
open Checking
open Inference
open Stateutils

type 'a filter_strategy = env -> evar_map -> 'a list -> evar_map * 'a list
type 'a stateless_filter_strategy = env -> evar_map -> 'a list -> 'a list

(* Filter trms to those that have type typ in env *)
let filter_by_type typ (env : env) sigma (trms : types list) =
  filter_state (fun tr sigma -> has_type env sigma typ tr) trms sigma

(* Find the singleton list with the first term that has type typ *)
let find_by_type typ (env : env) sigma (trms : types list) =
  try
    bind
      (find_state (fun tr sigma -> has_type env sigma typ tr) trms)
      (fun tr -> ret [tr])
      sigma
  with _ ->
    sigma, []

(* Filter a list of terms to those not exactly the same as the supplied term *)
let filter_not_same trm (_ : env) sigma (trms : types list) =
  let same = equal trm in (* exact equality for constructors *)
  sigma, List.filter (fun t -> not (same t)) trms

(*
 * Eliminate inductive hypotheses if possible.
 * This takes in a list of reduced candidates and filters out
 * the ones that still reference the IH.
 *
 * For now, only deals with candidates that refer explicitly to IH.
 * The ones that do will not pass the filter,
 * while the ones that don't will, and will then be type-checked.
 *
 * Sometimes this will not be possible, in which case we need a backup plan.
 * This is not yet implemented.
 *)
let filter_ihs (env : env) sigma (cs : types list) =
  let env_no_ih = pop_rel_context 1 env in
  filter_state
    (fun c sigma ->
      let c_no_ih = unshift c in
      try
        bind
          (fun sigma -> infer_type env_no_ih sigma c_no_ih)
          (fun _ -> ret true)
          sigma
      with _ ->
        sigma, false)
    cs
    sigma

(*
 * When we know sigma won't change, like in filter_not_same, ignore
 * sigma. If sigma does change, throw an error.
 *)
let filter_stateless f env sigma trms =
  let sigma', trms' = f env sigma trms in
  if sigma == sigma' then
    trms'
  else
    failwith "Can't call filter_stateless when the filter changes the evar_map"
