(*
 * Functions to manage the hypotheses of a term
 *)

open Constr
open Debruijn
open Termutils
open Envutils
open Environ
open Evd
open Zooming
open Typeutils

(* --- Eta expansion --- *)
               
(* Eta expansion of an application or function *)
let rec expand_eta (env : env) (evd : evar_map) (trm : types) : types =
  let typ = reduce_type env evd trm in
  let curried_args = mk_n_rels (arity typ) in
  reconstruct_lambda
    (zoom_env zoom_product_type empty_env typ)
    (mkAppl (shift_by (List.length curried_args) trm, curried_args))