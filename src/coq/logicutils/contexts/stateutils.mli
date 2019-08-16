(*
 * Utilities for evar_maps, which in Coq store state (evars, universe
 * constraints, and so on)
 *)

open Evd

(* --- State monad --- *)

type 'a state = 'a * evar_map

val bind :
  (evar_map -> 'a state) ->
  ('a -> evar_map -> 'b state) ->
  evar_map ->
  'b state

val ret : 'a -> evar_map -> 'a state

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

val map2_fold_state :
  evar_map ->
  (evar_map -> 'a -> 'b -> evar_map * 'c) ->
  'a list ->
  'b list ->
  evar_map * 'c list
  

val map_fold_state_array :
  evar_map ->
  (evar_map -> 'a -> evar_map * 'b) ->
  'a array ->
  evar_map * 'b array

val flat_map_fold_state :
  evar_map ->
  (evar_map -> 'a -> evar_map * 'b list) ->
  'a list ->
  evar_map * 'b list

val exists_state :
  evar_map ->
  (evar_map -> 'a -> evar_map * bool) ->
  'a list ->
  evar_map * bool

val find_state :
  evar_map ->
  (evar_map -> 'a -> evar_map * bool) ->
  'a list ->
  evar_map * 'a

val filter_state :
  evar_map ->
  (evar_map -> 'a -> evar_map * bool) ->
  'a list ->
  evar_map * 'a list
