(*
 * Utilities for evar_maps, which in Coq store state (evars, universe
 * constraints, and so on)
 *)

open Evd

(* --- State monad --- *)

(*
 * I'm actually rarely sold on this style of programming. But here I think
 * it can help define combinators that force good evar_map discipline.
 * I expose these because people might like to use them more generally.
 *)

type 'a state = evar_map * 'a
let bind f1 f2 = (fun sigma -> let sigma, a = f1 sigma in f2 a sigma) 
let ret a = fun sigma -> sigma, a 

(* --- Threading state through arguments --- *)

(*
 * For a function that takes and returns state, map that function over a 
 * list of arguments, threading the state through the application to the result.
 *
 * The order here is left-to-right since that is the way functions are applied 
 * in Coq (arguments may depend on earlier arguments). This is sometimes
 * significant.
 *)
let map_fold_state f l sigma =
  Util.on_snd
    List.rev
    (List.fold_left
       (fun (sigma, bs) a ->
         bind (f a) (fun b -> ret (b :: bs)) sigma)
       (sigma, [])
       l)

(*
 * map2 version
 *)
let map2_fold_state sigma f l1 l2 =
  Util.on_snd
    List.rev
    (List.fold_left2
       (fun (sigma, cs) a b ->
         let sigma, c = f a b sigma in
         sigma, c :: cs)
       (sigma, [])
       l1
       l2)

(*
 * Array version
 *)
let map_fold_state_array sigma f arr =
  Util.on_snd Array.of_list (map_fold_state f (Array.to_list arr) sigma)

(*
 * flat_map version
 *)
let flat_map_fold_state sigma f l =
  let sigma, l = map_fold_state f l sigma in
  Util.on_snd
    List.rev
    (List.fold_left
       (fun (sigma, bss) bs ->
         sigma, List.append bs bss)
       (sigma, [])
       l)

(*
 * Predicate version, for exists
 *)
let exists_state sigma p l =
  List.fold_left
    (fun (sigma, p_holds) a ->
      if p_holds then
        sigma, p_holds
      else
        p a sigma)
    (sigma, false)
    l

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
    (List.fold_left
      (fun (sigma, a_opt) a ->
        if Option.has_some a_opt then
          sigma, a_opt
        else
          let sigma, p_holds = p a sigma in
          if p_holds then
            sigma, Some a
          else
            sigma, None)
      (sigma, None)
      l)

(*
 * Filter
 *)
let filter_state sigma p l =
  List.fold_left
    (fun (sigma, a_l) a ->
      let sigma, p_holds = p a sigma in
      if p_holds then
        sigma, a :: a_l
      else
        sigma, a_l)
    (sigma, [])
    l
