(*
 * Utilities for evar_maps, which in Coq store state (evars, universe
 * constraints, and so on)
 *)

open Evd
open Utilities

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
let shas_some o = ret (Option.has_some o)
let ssome a = ret (Some a)
let snone = ret None
let sget o = ret (Option.get o)
      
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
 * flatten
 *)
let flatten_state l =
  bind (fold_left_state sappendr [] l) srev

(*
 * flat_map version
 *)
let flat_map_state f l =
  bind (map_state f l) flatten_state
       
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
 * Stateful and (pa a && pb b)
 *)
let and_state pa pb a b =
  branch_state pa (fun _ -> pb b) (fun _ -> ret false) a

(*
 * Stateful or (pa a || pb b)
 *)
let or_state pa pb a b =
  branch_state pa (fun _ -> ret true) (fun _ -> pb b) a

(*
 * Stateful not
 * Note that if p holds, this returns false and the evar_map from p
 * If p does not hold, this returns true and the evar_map argument
 *)
let not_state p a =
  branch_state p (fun _ -> ret false) (fun _ -> ret true) a
               
(*
 * Predicate version, for exists
 *)
let exists_state p l =
  fold_left_state
    (fun b -> branch_state (fun _ -> ret b) (fun _ -> ret b) p)
    false
    l

(*
 * Stateful forall
 *)
let forall_state p l =
  fold_left_state
    (fun b -> branch_state p (fun _ -> ret b) (fun _ -> ret false))
    true
    l

(*
 * Predicate version, for find
 *)
let find_state p l =
  bind
    (fold_left_state
       (fun o a ->
         branch_state
           shas_some
           ret
           (fun _ -> branch_state p ssome (fun _ -> snone) a)
           o)
       None
       l)
    (branch_state shas_some sget (fun _ _ -> raise Not_found))

(*
 * Filter
 *)
let filter_state p l =
  bind
    (fold_left_state
       (fun a_l ->
         branch_state
           p
           (sconsr a_l)
           (fun _ -> ret a_l))
       []
       l)
    srev

(*
 * Partition
 *)
let partition_state p l =
  bind
    (fold_left_state
       (fun (a_l1, a_l2) ->
         branch_state
           p
           (fun a -> ret (a :: a_l1, a_l2))
           (fun a -> ret (a_l1, a :: a_l2)))
       ([], [])
       l)
    (fun (l1, l2) -> ret (map_tuple List.rev (l1, l2)))
