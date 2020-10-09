(*
 * DeBruijn management
 *)

open Environ
open Constr

(* --- Numbers --- *)

(*
 * Shifting and unshifting by an amount
 *)
val unshift_i_by : int -> int -> int
val shift_i_by : int -> int -> int

(*
 * Shifting and unshifting
 *)
val unshift_i : int -> int 
val shift_i : int -> int

(* --- Terms --- *)

(*
 * Shifting and unshifting all indices greater than some amount by an amount
 *)
val unshift_local : int -> int -> types -> types
val shift_local : int -> int -> types -> types

(*
 * Shifting and unshifting all indices greater than 0 by an amount
 *)
val unshift_by : int -> types -> types
val shift_by : int -> types -> types

(*
 * Shifting and unshifting all indices greater than 0
 *)
val shift : types -> types
val unshift : types -> types

(* Shift everything and pray; workaround for bug *)
val shift_by_unconditional : int -> types -> types

(*
 * Gather the set of relative (de Bruijn) variables occurring in the term that
 * are free (i.e., not bound) under nb levels of external relative binding.
 *
 * Use free_rels 0 Int.Set.empty if you do not wish to filter out any free
 * relative variables below a certain binding level (nb) or supply the initial
 * accumulator (frels).
 *
 * Examples:
 * - free_rels 0 {} (Lambda(_, Rel 2, App(Rel 2, [Rel 1; Rel 4]))) = { 1, 2, 3 }
 * - free_rels 1 {} (Lambda(_, Rel 2, App(Rel 2, [Rel 1; Rel 4]))) = { 2, 3 }
 * - free_rels 2 {} (Lambda(_, Rel 2, App(Rel 2, [Rel 1; Rel 4]))) = { 3 }
 *
 *)
val free_rels : int -> Int.Set.t -> types -> Int.Set.t

(* --- Lists --- *)

(*
 * Shifting and unshifting a list 
 *)
val shift_all : types list -> types list
val unshift_all : types list -> types list

(*
 * Shifting and unshifting a list by an amount
 *)
val shift_all_by : int -> types list -> types list
val unshift_all_by : int -> types list -> types list

(* --- Substitutions --- *)

(*
 * Shifting a list of substitutions or its projections
 *)
val shift_subs : (types * types) list -> (types * types) list
val shift_from : (types * types) list -> (types * types) list
val shift_to : (types * types) list -> (types * types) list
                                         
(* --- Environments --- *)

(* Shift a term from the old (first) env to the new (second) env *)
val shift_to_env : (env * env) -> types -> types

(* Unshifts indexes for terms in an environment by an amount *)
val unshift_env_by : int -> env -> env

