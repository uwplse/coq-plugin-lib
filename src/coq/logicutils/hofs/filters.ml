(* Filters *)

open Constr
open Environ
open Debruijn
open Evd
open Utilities

type 'a filter_strategy = env -> evar_map -> 'a list -> 'a list

(* --- TODO for refactoring without breaking things --- *)

(*
 * Infer the type of trm in env
 * Note: This does not yet use good evar map hygeine; will fix that
 * during the refactor.
 *)
let infer_type (env : env) (evd : evar_map) (trm : types) : types =
  let jmt = Typeops.infer env trm in
  j_type jmt

(* Check whether two terms are convertible, ignoring universe inconsistency *)
let conv_ignoring_univ_inconsistency env evm (trm1 : types) (trm2 : types) : bool =
  match map_tuple kind (trm1, trm2) with
  | (Sort (Type u1), Sort (Type u2)) ->
     (* PUMPKIN assumes universe consistency for now *)
     true
  | _ ->
     let etrm1 = EConstr.of_constr trm1 in
     let etrm2 = EConstr.of_constr trm2 in
     try
       Reductionops.is_conv env evm etrm1 etrm2
     with _ ->
       false

(* Checks whether two terms are convertible in env with no evars *)
let convertible (env : env) (evd : evar_map) (trm1 : types) (trm2 : types) : bool =
  conv_ignoring_univ_inconsistency env Evd.empty trm1 trm2

(* Check whether a term has a given type *)
let has_type (env : env) (evd : evar_map) (typ : types) (trm : types) : bool =
  try
    let trm_typ = infer_type env evd trm in
    convertible env evd trm_typ typ
  with _ -> false
               
(* --- End TODO --- *)

(* Filter trms to those that have type typ in env *)
let filter_by_type typ (env : env) (evd : evar_map) (trms : types list) : types list =
  try
    List.filter (has_type env evd typ) trms
  with
  | _ -> []

(* Find the singleton list with the first term that has type typ *)
let find_by_type typ (env : env) (evd : evar_map) (trms : types list) : types list =
  try
    [List.find (has_type env evd typ) trms]
  with
  | _ -> []

(* Filter a list of terms to those not exactly the same as the supplied term *)
let filter_not_same trm (_ : env) (_ : evar_map) (trms : types list) : types list =
  let same = equal trm in (* exact equality for constructors *)
  List.filter (fun t -> not (same t)) trms

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
let filter_ihs (env : env) (evd : evar_map) (cs : types list) : types list =
  let env_no_ih = pop_rel_context 1 env in
  List.filter
    (fun c ->
      let c_no_ih = unshift c in
      try
        ignore (infer_type env_no_ih evd c_no_ih);
        true
      with _ -> false)
    cs
