(*
 * Zooming into environments and reconstructing terms from environments
 *)

open EConstr
open Environ
open Apputils
open Funutils
open Envutils
open Contextutils
open Debruijn
open Sigmautils
open Evd

(* --- Zooming --- *)

(* Zoom into a term *)
let rec zoom_n_prod env sigma npm typ : env * types =
  if npm = 0 then
    (env, typ)
  else
    match kind sigma typ with
    | Prod (n1, t1, b1) ->
       zoom_n_prod (push_local (n1, t1) env) sigma (npm - 1) b1
    | _ ->
       failwith "more parameters expected"

(* Lambda version *)
let zoom_n_lambda env sigma npm trm : env * types =
  let (env, typ) = zoom_n_prod env sigma npm (lambda_to_prod sigma trm) in
  (env, prod_to_lambda sigma typ)

(* Zoom all the way into a lambda term *)
let rec zoom_lambda_term (env : env) sigma (trm : types) : env * types =
  match kind sigma trm with
  | Lambda (n, t, b) ->
     zoom_lambda_term (push_local (n, t) env) sigma b
  | _ ->
     (env, trm)

(* Zoom all the way into a product type *)
let rec zoom_product_type (env : env) sigma (typ : types) : env * types =
  match kind sigma typ with
  | Prod (n, t, b) ->
     zoom_product_type (push_local (n, t) env) sigma b
  | _ ->
     (env, typ)

(* Zoom into the environment *)
let zoom_env zoom (env : env) sigma (trm : types) : env =
  fst (zoom env sigma trm)

(* Zoom into the term *)
let zoom_term zoom (env : env) sigma (trm : types) : types =
  snd (zoom env sigma trm)

(* Get the last argument of a sigma *)
let zoom_sig_lambda sigma t =
  last_arg sigma t

(* Get the application from the body of the last argument of a sigma *)
let zoom_sig_app sigma t =
  let lambda = zoom_sig_lambda sigma t in
  zoom_term zoom_lambda_term empty_env sigma lambda

(* Get the very first function from the body of the last argument of a sigma *)
let zoom_sig sigma t =
  first_fun sigma (zoom_sig_app sigma t)

(* zoom_sig if t actually applies sigT *)
let zoom_if_sig_lambda sigma t =
  if applies sigma sigT t then
    zoom_sig_lambda sigma t
  else
    t

(* zoom_sig_app if actually applies sigT *)
let zoom_if_sig_app sigma t =
  if applies sigma sigT t then
    zoom_sig_app sigma t
  else
    t

(* zoom if t actually applies sigT *)
let zoom_if_sig sigma t =
  if applies sigma sigT t then
    zoom_sig sigma t
  else
    t

(* --- Reconstruction --- *)

(* Reconstruct a lambda from an environment, but stop when i are left *)
let rec reconstruct_lambda_n (env : env) sigma (b : types) (i : int) : types =
  if nb_rel env = i then
    b
  else
    let (n, _, t) = CRD.to_tuple @@ EConstr.lookup_rel 1 env in
    let env' = pop_rel_context 1 env in
    reconstruct_lambda_n env' sigma (mkLambda (n, t, b)) i

(* Reconstruct a lambda from an environment *)
let reconstruct_lambda (env : env) sigma (b : types) : types =
  reconstruct_lambda_n env sigma b 0

(* Like reconstruct_lambda_n, but first skip j elements *)
let rec reconstruct_lambda_n_skip (env : env) sigma (b : types) (i : int) (j : int) : types =
  if nb_rel env = i then
    b
  else
    let (n, _, t) = CRD.to_tuple @@ EConstr.lookup_rel 1 env in
    let env' = pop_rel_context 1 env in
    if j <= 0 then
      reconstruct_lambda_n_skip env' sigma (mkLambda (n, t, b)) i j
    else
      reconstruct_lambda_n_skip env' sigma (unshift sigma b) (i - 1) (j - 1)
                

(* Reconstruct a product from an environment, but stop when i are left *)
let rec reconstruct_product_n (env : env) sigma (b : types) (i : int) : types =
  if nb_rel env = i then
    b
  else
    let (n, _, t) = CRD.to_tuple @@ EConstr.lookup_rel 1 env in
    let env' = pop_rel_context 1 env in
    reconstruct_product_n env' sigma (mkProd (n, t, b)) i

(* Reconstruct a product from an environment *)
let reconstruct_product (env : env) sigma (b : types) : types =
  reconstruct_product_n env sigma b 0

(* Like reconstruct_product_n, but first skip j elements *)
let rec reconstruct_product_n_skip (env : env) sigma (b : types) (i : int) (j : int) : types =
  if nb_rel env = i then
    b
  else
    let (n, _, t) = CRD.to_tuple @@ EConstr.lookup_rel 1 env in
    let env' = pop_rel_context 1 env in
    if j <= 0 then
      reconstruct_product_n_skip env' sigma (mkProd (n, t, b)) i j
    else
      reconstruct_product_n_skip env' sigma (unshift sigma b) (i - 1) (j - 1)

(* --- Higher-order zooming --- *)

(*
 * Zoom in and apply a function
 *)
let in_body zoom f env sigma trm =
  let (env_body, trm_body) = zoom env sigma trm in
  f env_body sigma trm_body

let in_lambda_body f env sigma trm = in_body zoom_lambda_term f env sigma trm

(*
 * Zoom in, apply a function, then reconstruct the result
 *)
let zoom_apply zoom reconstruct f =
  in_body
    zoom
    (fun env sigma trm ->
      let sigma, trm = f env sigma trm in
      sigma, reconstruct env sigma trm)

let zoom_apply_lambda =
  zoom_apply zoom_lambda_term reconstruct_lambda

let zoom_apply_lambda_empty f trm =
  snd
    (zoom_apply
       zoom_lambda_term
       reconstruct_lambda
       (fun _ sigma trm -> sigma, f trm)
       empty_env
       Evd.empty
       trm)
             
let zoom_apply_lambda_n n =
  zoom_apply zoom_lambda_term (fun e sigma t -> reconstruct_lambda_n e sigma t n)

let zoom_apply_lambda_n_skip n skip =
  zoom_apply zoom_lambda_term (fun e sigma t -> reconstruct_lambda_n_skip e sigma t n skip)

