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

(* Shift everything and pray; workaround for bug *)
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
