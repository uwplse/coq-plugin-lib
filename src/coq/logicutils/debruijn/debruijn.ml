(* --- DeBruijn management --- *)

open Environ
open Constr
open Utilities
open Envutils
open Hofs

(* --- Numbers --- *)

(* Unshift an index by n *)
let unshift_i_by (n : int) (i : int) : int =
  i - n

(* Shift an index by n *)
let shift_i_by (n : int) (i : int) : int =
  unshift_i_by (- n) i

(* Unshift an index *)
let unshift_i (i : int) : int =
  unshift_i_by 1 i

(* Shift an index *)
let shift_i (i : int) : int =
  shift_i_by 1 i

(* --- Terms --- *)

(*
 * Unshifts a term by n if it is greater than the maximum index
 * max of a local binding
 *)
let unshift_local (max : int) (n : int) (trm : types) : types =
  map_term
    (fun (m, adj) t ->
      match kind t with
      | Rel i ->
         let i' = if i > m then unshift_i_by adj i else i in
         mkRel i'
      | _ ->
         t)
    (fun (m, adj) -> (shift_i m, adj))
    (max, n)
    trm

(*
 * Shifts a term by n if it is greater than the maximum index
 * max of a local binding
 *)
let shift_local (max : int) (n : int) (trm : types) : types =
  unshift_local max (- n) trm

(* Decrement the relative indexes of a term t by n *)
let unshift_by (n : int) (trm : types) : types =
  unshift_local 0 n trm

(* Increment the relative indexes of a term t by n *)
let shift_by (n : int) (t : types) : types  =
  unshift_by (- n) t

(* Increment the relative indexes of a term t by one *)
let shift (t : types) : types  =
  shift_by 1 t

(* Decrement the relative indexes of a term t by one *)
let unshift (t : types) : types =
  unshift_by 1 t

(* Shift everything and pray; workaround for bug (TODO investigate) *)
let shift_by_unconditional (n : int) (trm : types) : types =
  map_term
    (fun _ t ->
      match kind t with
      | Rel i ->
         let i' = shift_i_by n i in
         mkRel i'
      | _ ->
         t)
    (fun _ -> ())
    ()
    trm

(*
 * Function from: 
 * https://github.com/coq/coq/commit/7ada864b7728c9c94b7ca9856b6b2c89feb0214e
 * Inlined here to make this compatible with Coq 8.8.0
 * TODO remove with update
 *)
let fold_constr_with_binders g f n acc c =
  match kind c with
  | (Rel _ | Meta _ | Var _   | Sort _ | Const _ | Ind _
    | Construct _) -> acc
  | Cast (c,_, t) -> f n (f n acc c) t
  | Prod (na,t,c) -> f (g  n) (f n acc t) c
  | Lambda (na,t,c) -> f (g  n) (f n acc t) c
  | LetIn (na,b,t,c) -> f (g  n) (f n (f n acc b) t) c
  | App (c,l) -> Array.fold_left (f n) (f n acc c) l
  | Proj (p,c) -> f n acc c
  | Evar (_,l) -> Array.fold_left (f n) acc l
  | Case (_,p,c,bl) -> Array.fold_left (f n) (f n (f n acc p) c) bl
  | Fix (_,(lna,tl,bl)) ->
      let n' = CArray.fold_left2 (fun c n t -> g c) n lna tl in
      let fd = Array.map2 (fun t b -> (t,b)) tl bl in
      Array.fold_left (fun acc (t,b) -> f n' (f n acc t) b) acc fd
  | CoFix (_,(lna,tl,bl)) ->
      let n' = CArray.fold_left2 (fun c n t -> g c) n lna tl in
      let fd = Array.map2 (fun t b -> (t,b)) tl bl in
      Array.fold_left (fun acc (t,b) -> f n' (f n acc t) b) acc fd

(*
 * Gather the set of relative (de Bruijn) variables occurring in the term that
 * are free (i.e., not bound) under nb levels of external relative binding.
 *
 * Use free_rels 0 Int.Set.empty if you do not wish to filter out any free
 * relative variables below a certain binding level (nb) or supply the initial
 * accumulator (frels).
 *
 * Examples:
 * - free_rels 0 (Lambda(_, Rel 2, App(Rel 2, [Rel 1; Rel 4]))) = { 1, 2, 3 }
 * - free_rels 1 (Lambda(_, Rel 2, App(Rel 2, [Rel 1; Rel 4]))) = { 2, 3 }
 * - free_rels 2 (Lambda(_, Rel 2, App(Rel 2, [Rel 1; Rel 4]))) = { 3 }
 *
 * Like many functions, by Nate Yazdani from original DEVOID code
 *)
let rec free_rels nb frels term =
  match Constr.kind term with
  | Rel i ->
    if i > nb then Int.Set.add (unshift_i_by nb i) frels else frels
  | _ ->
    fold_constr_with_binders succ free_rels nb frels term

(* --- Lists --- *)

(* Shift a list *)
let shift_all = List.map shift

(* Shift all elements of a list by n *)
let shift_all_by n = List.map (shift_by n)

(* Unshift a list *)
let unshift_all = List.map unshift

(* Unshift all elements of a list by n *)
let unshift_all_by n = List.map (unshift_by n)

(* --- Substitutions --- *)

(* Shift substitutions *)
let shift_subs = List.map (map_tuple shift)

(* Shift from substitutions *)
let shift_from = List.map (fun (s, d) -> (shift s, d))

(* Shift to substitutions *)
let shift_to = List.map (fun (s, d) -> (s, shift d))
                                
(* --- Environments --- *)

(* Shift a term by the offset from env_o to env_n *)
let shift_to_env (env_o, env_n) trm =
  shift_by (new_rels2 env_n env_o) trm

(* Unshifts indexes for terms in env by n *)
let unshift_env_by (n : int) (env : env) : env =
  let num_rels = nb_rel env in
  let all_relis = List.rev (from_one_to num_rels) in
  let all_rels = lookup_rels all_relis env in
  List.fold_left
    (fun env decl ->
      push_rel decl env)
    (pop_rel_context num_rels env)
    all_rels

