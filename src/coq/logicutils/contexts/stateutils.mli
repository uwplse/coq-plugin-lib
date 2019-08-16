(*
 * Utilities for evar_maps, which in Coq store state (evars, universe
 * constraints, and so on)
 *)

open Evd

(* --- State monad --- *)

(*
 * Putting the evar_map first here is deliberate for consistency with other
 * functions from the Coq library.
 *)
type 'a state = evar_map * 'a

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
  ('a -> evar_map -> 'b state) ->
  'a list ->
  evar_map ->
  ('b list) state

val map2_fold_state :
  evar_map ->
  ('a -> 'b -> evar_map -> 'c state) ->
  'a list ->
  'b list ->
  ('c list) state

val map_fold_state_array :
  evar_map ->
  ('a -> evar_map -> 'b state) ->
  'a array ->
  ('b array) state

val flat_map_fold_state :
  evar_map ->
  ('a -> evar_map -> ('b list) state) ->
  'a list ->
  ('b list) state

val exists_state :
  evar_map ->
  ('a -> evar_map -> bool state) ->
  'a list ->
  bool state

val find_state :
  evar_map ->
  ('a -> evar_map -> bool state) ->
  'a list ->
  'a state

val filter_state :
  evar_map ->
  ('a -> evar_map -> bool state) ->
  'a list ->
  ('a list) state
