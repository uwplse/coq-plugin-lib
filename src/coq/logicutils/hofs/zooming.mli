(*
 * Zooming into environments and reconstructing terms from environments
 *)

open EConstr
open Environ
open Evd
                 
(* --- Zoom n deep --- *)

val zoom_n_prod : env -> evar_map -> int -> types -> (env * types)
val zoom_n_lambda : env -> evar_map -> int -> types -> (env * types)

(* --- Zoom all the way --- *)
                                             
val zoom_lambda_term : env -> evar_map -> types -> (env * types)
val zoom_product_type : env -> evar_map -> types -> (env * types)

(* --- Projections of zooming --- *)
                                          
val zoom_env : (env -> evar_map -> types -> (env * types)) -> env -> evar_map -> types -> env
val zoom_term : (env -> evar_map -> types -> (env * types)) -> env -> evar_map -> types -> types

(* --- Zooming for sigma types --- *)

val zoom_sig_lambda : evar_map -> types -> types
val zoom_sig_app : evar_map -> types -> types
val zoom_sig : evar_map -> types -> types

(* --- Conditional zooming for sigma types --- *)

val zoom_if_sig_lambda : evar_map -> types -> types
val zoom_if_sig_app : evar_map -> types -> types
val zoom_if_sig : evar_map -> types -> types
                             
(* --- Reconstruct until n are left --- *)
                                                                     
val reconstruct_lambda_n : env -> evar_map -> types -> int -> types
val reconstruct_product_n : env -> evar_map -> types -> int -> types

(* --- Reconstruct until n are left, skipping a given amount first --- *)
                                                     
val reconstruct_lambda_n_skip : env -> evar_map -> types -> int -> int -> types
val reconstruct_product_n_skip : env -> evar_map -> types -> int -> int -> types

(* --- Reconstruct fully --- *)
                                                                 
val reconstruct_lambda : env -> evar_map -> types -> types
val reconstruct_product : env -> evar_map -> types -> types

(* --- Zoom in and apply a function --- *)

val in_body :
  (env -> evar_map -> types -> (env * types)) ->
  (env -> evar_map -> types -> 'a) ->
  env ->
  evar_map ->
  types ->
  'a

val in_lambda_body :
  (env -> evar_map -> types -> 'a) ->
  env ->
  evar_map ->
  types ->
  'a
                                            
(* --- Zoom in, apply a function, then reconstruct the result --- *)

val zoom_apply :
  (env -> evar_map -> types -> (env * types)) -> (* zoomer *)
  (env -> evar_map -> types -> types) -> (* reconstructer *)
  (env -> evar_map -> types -> evar_map * types) -> (* function *)
  env ->
  evar_map ->
  types ->
  evar_map * types

val zoom_apply_lambda :
  (env -> evar_map -> types -> evar_map * types) ->
  env ->
  evar_map ->
  types ->
  evar_map * types

val zoom_apply_lambda_empty :
  (types -> types) -> types -> types

val zoom_apply_lambda_n :
  int ->
  (env -> evar_map -> types -> evar_map * types) ->
  env ->
  evar_map ->
  types ->
  evar_map * types

val zoom_apply_lambda_n_skip :
  int ->
  int ->
  (env -> evar_map -> types -> evar_map * types) ->
  env ->
  evar_map ->
  types ->
  evar_map * types

