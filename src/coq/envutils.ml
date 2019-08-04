(*
 * Utility functions for environments
 *)

open Utilities
open Environ
open Constr
open Declarations
open Decl_kinds
open Constrextern
open Termutils
open Contextutils

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

(* Lookup n rels and remove then *)
let lookup_pop (n : int) (env : env) =
  let rels = List.map (fun i -> lookup_rel i env) (from_one_to n) in
  (pop_rel_context n env, rels)

let force_constant_body const_body =
  match const_body.const_body with
  | Def const_def ->
    Mod_subst.force_constr const_def
  | OpaqueDef opaq ->
    Opaqueproof.force_proof (Global.opaque_tables ()) opaq
  | _ ->
    CErrors.user_err ~hdr:"force_constant_body"
      (Pp.str "An axiom has no defining term")

(* Lookup a definition *)
let lookup_definition (env : env) (def : types) : types =
  match kind def with
  | Const (c, u) -> force_constant_body (lookup_constant c env)
  | Ind _ -> def
  | _ -> failwith "not a definition"

(* Fully lookup a def in env, but return the term if it is not a definition *)
let rec unwrap_definition (env : env) (trm : types) : types =
  try
    unwrap_definition env (lookup_definition env trm)
  with _ ->
    trm

(* Find the offset of some environment from some number of parameters *)
let new_rels env npm = nb_rel env - npm

(* Find the offset between two environments *)
let new_rels2 env1 env2 = nb_rel env1 - nb_rel env2
