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
 * map2 version
 *)
let map2_fold_state sigma f l1 l2 =
  List.fold_right2
    (fun a b (sigma, cs) ->
      let sigma, c = f sigma a b in
      sigma, c :: cs)
    l1
    l2
    (sigma, [])

(*
 * Array version
 *)
let map_fold_state_array sigma f arr =
  Util.on_snd Array.of_list (map_fold_state sigma f (Array.to_list arr))

(*
 * flat_map version
 *)
let flat_map_fold_state sigma f l =
  let sigma, l = map_fold_state sigma f l in
  List.fold_right
    (fun bs (sigma, bss) ->
      sigma, List.append bs bss)
    l
    (sigma, [])

(*
 * Predicate version, for exists
 *)
let exists_state sigma p l =
  List.fold_right
    (fun a (sigma, p_holds) ->
      if p_holds then
        sigma, p_holds
      else
        p sigma a)
    l
    (sigma, false)

(*
 * Predicate version, for find
 *)
let find_state sigma p l =
  Util.on_snd
    (fun a_opt ->
      if Option.has_some a_opt then
        Option.get a_opt
      else
        raise Not_found)
    (List.fold_right
      (fun a (sigma, a_opt) ->
        if Option.has_some a_opt then
          sigma, a_opt
        else
          let sigma, p_holds = p sigma a in
          if p_holds then
            sigma, Some a
          else
            sigma, None)
      l
      (sigma, None))

(*
 * Filter
 *)
let filter_state sigma p l =
  List.fold_right
    (fun a (sigma, a_l) ->
      let sigma, p_holds = p sigma a in
      if p_holds then
        sigma, a :: a_l
      else
        sigma, a_l)
    l
    (sigma, [])
