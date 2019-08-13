(*
 * Utilities for evar_maps, which in Coq store state (evars, universe
 * constraints, and so on)
 *)

open Evd

(* --- Threading state through arguments --- *)

(*
 * For a function that takes and returns state, map that function over a 
 * list of arguments, threading the state through the application to the result
 *)
val map_fold_state :
  evar_map ->
  (evar_map -> 'a -> evar_map * 'b) ->
  'a list ->
  evar_map * 'b list

val map_fold_state_array :
  evar_map ->
  (evar_map -> 'a -> evar_map * 'b) ->
  'a array ->
  evar_map * 'b array
