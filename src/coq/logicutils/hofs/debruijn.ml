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
let unshift_local (env: env) (max : int) (n : int) (trm : types) : types =
  map_term env
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
let shift_local (env: env) (max : int) (n : int) (trm : types) : types =
  unshift_local env max (- n) trm

(* Decrement the relative indexes of a term t by n *)
let unshift_by (env: env) (n : int) (trm : types) : types =
  unshift_local env 0 n trm

(* Increment the relative indexes of a term t by n *)
let shift_by (env: env) (n : int) (t : types) : types  =
  unshift_by env (- n) t

(* Increment the relative indexes of a term t by one *)
let shift (env: env) (t : types) : types  =
  shift_by env 1 t

(* Decrement the relative indexes of a term t by one *)
let unshift (env: env) (t : types) : types =
  unshift_by env 1 t

(* Shift everything and pray; workaround for bug (TODO investigate) *)
let shift_by_unconditional (env: env) (n : int) (trm : types) : types =
  map_term env
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
let shift_all env = List.map (shift env)

(* Shift all elements of a list by n *)
let shift_all_by env n = List.map (shift_by env n)

(* Unshift a list *)
let unshift_all env = List.map (unshift env)

(* Unshift all elements of a list by n *)
let unshift_all_by env n = List.map (unshift_by env n)

(* --- Substitutions --- *)

(* Shift substitutions *)
let shift_subs env = List.map (map_tuple (shift env))

(* Shift from substitutions *)
let shift_from env = List.map (fun (s, d) -> (shift env s, d))

(* Shift to substitutions *)
let shift_to env = List.map (fun (s, d) -> (s, shift env d))
                                
(* --- Environments --- *)

(* Shift a term by the offset from env_o to env_n *)
let shift_to_env (env_o, env_n) trm =
  shift_by env_n (new_rels2 env_n env_o) trm

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

