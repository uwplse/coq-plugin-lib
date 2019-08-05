(*
 * Utility functions for environments
 * TODO organize
 *)

open Environ
open Constr
open Names
open Contextutils

(* Look up all indexes from a list in an environment *)
val lookup_rels : int list -> env -> CRD.t list

(* Return a list of all bindings in an environment, starting with the closest *)
val lookup_all_rels : env -> CRD.t list
                                                       
(* Return a list of all indexes in an environment, starting with 1 *)
val all_rel_indexes : env -> int list

(* Return a list of relative indexes, from highest to lowest, of size n *)
val mk_n_rels : int -> types list

(*
 * Push to an environment
 *)
val push_local : (name * types) -> env -> env
val push_let_in : (name * types * types) -> env -> env

(*
 * Lookup from an environment
 *)
val lookup_pop : int -> env -> (env * CRD.t list)
val lookup_definition : env -> types -> types
val unwrap_definition : env -> types -> types

(*
 * Offset between an environment and an index, or two environments, respectively
 *)
val new_rels : env -> int -> int
val new_rels2 : env -> env -> int
