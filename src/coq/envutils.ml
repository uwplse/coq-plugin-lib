(*
 * Utility functions for environments
 *)

open Utilities
open Environ
open Constr

module CRD = Context.Rel.Declaration

(* Look up all indexes from is in env *)
let lookup_rels (is : int list) (env : env) : CRD.t list =
 List.map (fun i -> lookup_rel i env) is

(* Return a list of all indexes in env, starting with 1 *)
let all_rel_indexes (env : env) : int list =
  from_one_to (nb_rel env)

(* Make n relative indices, from highest to lowest *)
let mk_n_rels n =
  List.map mkRel (List.rev (from_one_to n))

(* Return a list of all bindings in env, starting with the closest *)
let lookup_all_rels (env : env) : CRD.t list =
  lookup_rels (all_rel_indexes env) env

(* Push a local binding to an environment *)
let push_local (n, t) = push_rel CRD.(LocalAssum (n, t))

(* Push a let-in definition to an environment *)
let push_let_in (n, e, t) = push_rel CRD.(LocalDef(n, e, t))
