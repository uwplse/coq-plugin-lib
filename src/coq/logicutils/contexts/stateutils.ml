(*
 * Utilities for evar_maps, which in Coq store state (evars, universe
 * constraints, and so on)
 *
 * TODO see if these exist in Coq API
 *)

open Evd

(* --- Threading state through arguments --- *)

(*
 * For a function that takes and returns state, map that function over a 
 * list of arguments, threading the state through the application to the result.
 *
 * The order here is left-to-right since that is the way functions are applied 
 * in Coq (arguments may depend on earlier arguments). This is sometimes
 * significant.
 *)
let map_fold_state sigma f l =
  List.fold_right
    (fun a (sigma, bs) ->
      let sigma, b = f sigma a in
      sigma, b :: bs)
    l
    (sigma, [])

(*
 * Array version
 *)
let map_fold_state_array sigma f arr =
  Util.on_snd Array.of_list (map_fold_state sigma f (Array.to_list arr))

(* --- Other utilities --- *)              

(*
 * Ignore the state in the argument
 *)
let ignore_state f =
  (fun _ -> f)
