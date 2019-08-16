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

(* Internal utilities *)
let sconsr bs b = ret (b :: bs)
let srev l = ret (List.rev l)
let sarray_of_list l = ret (Array.of_list l)
let sappendr l1 l2 = ret (List.append l1 l2)
      
(*
 * fold_left with state
 *)
let fold_left_state f b l sigma =
  List.fold_left (fun (sigma, b) a -> f b a sigma) (ret b sigma) l

(*
 * fold_left with state
 *)
let fold_left2_state f c l1 l2 sigma =
  List.fold_left2 (fun (sigma, c) a b -> f c a b sigma) (ret c sigma) l1 l2

(*
 * For a function that takes and returns state, map that function over a 
 * list of arguments, threading the state through the application to the result.
 *
 * The order here is left-to-right since that is the way functions are applied 
 * in Coq (arguments may depend on earlier arguments). This is sometimes
 * significant.
 *)
let map_state f l =
  bind (fold_left_state (fun bs a -> bind (f a) (sconsr bs)) [] l) srev

(*
 * map2 version
 *)
let map2_state f l1 l2 =
  bind (fold_left2_state (fun cs a b -> bind (f a b) (sconsr cs)) [] l1 l2) srev

(*
 * Array version
 *)
let map_state_array f arr =
  bind (map_state f (Array.to_list arr)) sarray_of_list

(*
 * flat_map version
 *)
let flat_map_state f l =
  bind (bind (map_state f l) (fold_left_state sappendr [])) srev
       
(*
 * Stateful if/else
 *)
let branch_state p f g a =
  bind
    (fun sigma_f ->
      bind
        (p a)
        (fun b sigma_t -> ret b (if b then sigma_t else sigma_f))
        sigma_f)
    (fun b -> if b then f a else g a)

(*
 * Predicate version, for exists
 *)
let exists_state p l =
  fold_left_state
    (fun b -> branch_state (fun _ -> ret b) (fun _ -> ret b) p)
    false
    l

(*
 * Predicate version, for find
 *)
let find_state p l sigma =
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
          let sigma_t, p_holds = p a sigma in
          if p_holds then
            sigma_t, Some a
          else
            sigma, None)
      (sigma, None)
      l)

(*
 * Filter
 *)
let filter_state p l sigma =
  List.fold_left
    (fun (sigma, a_l) a ->
      let sigma, p_holds = p a sigma in
      if p_holds then
        sigma, a :: a_l
      else
        sigma, a_l)
    (sigma, [])
    l
