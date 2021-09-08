(*
 * Utility functions for environments
 *)

open Utilities
open Environ
open Constr
open Declarations
open Decl_kinds
open Constrextern
open Contextutils
open Evd
open Names
open Nameutils
open Stateutils
   
(* Look up all indexes from is in env *)
let lookup_rels (is : int list) (env : env) : rel_declaration list =
 List.map (fun i -> lookup_rel i env) is

(* Return a list of all indexes in env, starting with 1 *)
let all_rel_indexes (env : env) : int list =
  from_one_to (nb_rel env)

(* Make n relative indices, from highest to lowest *)
let mk_n_rels n =
  List.map mkRel (List.rev (from_one_to n))

(* Return a list of all bindings in env, starting with the closest *)
let lookup_all_rels (env : env) : rel_declaration list =
  lookup_rels (all_rel_indexes env) env

(* Return a name-type pair from the given rel_declaration. *)
let rel_name_type rel : Name.t * types =
  match rel with
  | CRD.LocalAssum (n, t) -> (n, t)
  | CRD.LocalDef (n, _, t) -> (n, t)


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
    let body = lookup_definition env trm in
    if equal body trm then
      trm
    else
      unwrap_definition env body
  with _ ->
    trm

(* Find the offset of some environment from some number of parameters *)
let new_rels env npm = nb_rel env - npm

(* Find the offset between two environments *)
let new_rels2 env1 env2 = nb_rel env1 - nb_rel env2

(* Ignore the environment and evar_map in a function *)
let ignore_env (f : 'a -> 'b) : env -> evar_map -> 'a -> 'b =
  (fun _ _ -> f)

(* Finds all rel names pushed onto an environment. *)
let get_pushed_names env : Id.Set.t =
  let names = List.map (fun x -> fst (rel_name_type x))
                (lookup_all_rels env) in
  Id.Set.of_list (List.map expect_name names)

(* If the given name is anonymous, generate a fresh one. *)
let fresh_name env n =
  let in_env = get_pushed_names env in
  let name = match n with
    | Anonymous -> Id.of_string "H"
    | Name n -> n in
  Tactics.fresh_id_in_env in_env name env


(* Returns true if the relative bindings in each environment
   are syntactically equal. *)
let compare_envs env1 env2 sigma =
  let rels1 = lookup_all_rels env1 in
  let rels2 = lookup_all_rels env2 in
  if List.length rels1 = List.length rels2 then
    fold_left2_state
      (fun b rel1 rel2 sigma ->
        sigma,
        b &&  match rel1, rel2 with
              | CRD.LocalAssum (n1, t1), CRD.LocalAssum (n2, t2)
                -> equal t1 t2
              | CRD.LocalDef (n1, t1, b1), CRD.LocalDef (n2, t2, b2)
                -> equal t1 t2 && equal b1 b2
              | _ -> false)
      true
      rels1
      rels2
      sigma
  else
    sigma, false
