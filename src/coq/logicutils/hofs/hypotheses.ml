(*
 * Functions to manage the hypotheses of a term
 * TODO how does this relate to Nate's eta?
 *)

open Constr
open Debruijn
open Envutils
open Environ
open Evd
open Zooming
open Apputils
open Reducers
open Funutils

(* --- Eta expansion --- *)
               
(* Eta expansion of an application or function (TODO do we need sigma?) *)
let expand_eta (env : env) sigma (trm : types) : evar_map * types =
  let sigma, typ = reduce_type env sigma trm in
  let curried_args = mk_n_rels (arity typ) in
  sigma, reconstruct_lambda
    (zoom_env zoom_product_type empty_env typ)
    (mkAppl (shift_by (List.length curried_args) trm, curried_args))
