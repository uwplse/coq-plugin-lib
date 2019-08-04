(*
 * Utilities for contexts
 *)

open Constr
open Names
open Environ
open Inference
open Declarations

module CRD = Context.Rel.Declaration

(* --- Constructing declarations --- *)

(* Make the rel declaration for a local assumption *)
let rel_assum (name, typ) = CRD.LocalAssum (name, typ)

(* Make the rel declaration for a local definition *)
let rel_defin (name, def, typ) = CRD.LocalDef (name, def, typ)               

(* --- Questions about declarations --- *)

(* Is the rel declaration a local assumption? *)
let is_rel_assum = CRD.is_local_assum

(* Is the rel declaration a local definition? *)
let is_rel_defin = CRD.is_local_def

(* --- Deconstructing declarations --- *)

(* Get the name of a rel declaration *)
let rel_name decl = CRD.get_name decl

(* Get the optional value of a rel declaration *)
let rel_value decl = CRD.get_value decl

(* Get the type of a rel declaration *)
let rel_type decl = CRD.get_type decl

(* --- Mapping over contexts --- *)

(* Map over a rel context with environment kept in sync *)
let map_rel_context env make ctxt =
  Context.Rel.fold_outside
    (fun decl (env, res) ->
       push_rel decl env, (make env decl) :: res)
    ctxt
    ~init:(env, []) |>
  snd

(* --- Binding in contexts --- *)

(*
 * Bind all local declarations in the relative context onto the body term as
 * products, substituting away (i.e., zeta-reducing) any local definitions.
 *)
let smash_prod_assum ctxt body =
  Context.Rel.fold_inside
    (fun body decl ->
       match rel_value decl with
       | Some defn -> Vars.subst1 defn body
       | None -> mkProd (rel_name decl, rel_type decl, body))
    ~init:body
    ctxt

(*
 * Bind all local declarations in the relative context onto the body term as
 * lambdas, substituting away (i.e., zeta-reducing) any local definitions.
 *)
let smash_lam_assum ctxt body =
  Context.Rel.fold_inside
    (fun body decl ->
       match rel_value decl with
       | Some defn -> Vars.subst1 defn body
       | None -> mkLambda (rel_name decl, rel_type decl, body))
    ~init:body
    ctxt

(*
 * Decompose the first n product bindings, zeta-reducing let bindings to reveal
 * further product bindings when necessary.
 *)
let decompose_prod_n_zeta n term =
  assert (n >= 0);
  let rec aux n ctxt body =
    if n > 0 then
      match Constr.kind body with
      | Prod (name, param, body) ->
        aux (n - 1) (Context.Rel.add (rel_assum (name, param)) ctxt) body
      | LetIn (name, def_term, def_type, body) ->
        aux n ctxt (Vars.subst1 def_term body)
      | _ ->
        invalid_arg "decompose_prod_n_zeta: not enough products"
    else
      ctxt, body
  in
  aux n Context.Rel.empty term

(*
 * Decompose the first n lambda bindings, zeta-reducing let bindings to reveal
 * further lambda bindings when necessary.
 *)
let decompose_lam_n_zeta n term =
  assert (n >= 0);
  let rec aux n ctxt body =
    if n > 0 then
      match Constr.kind body with
      | Lambda (name, param, body) ->
        aux (n - 1) (Context.Rel.add (rel_assum (name, param)) ctxt) body
      | LetIn (name, def_term, def_type, body) ->
        Vars.subst1 def_term body |> aux n ctxt
      | _ ->
        invalid_arg "decompose_lam_n_zeta: not enough lambdas"
    else
      ctxt, body
  in
  aux n Context.Rel.empty term

(* --- Getting bindings for certain kinds of terms --- *)

(*
 * Certain terms create bindings you need to push to the environment as you
 * recurse. These functions give you those bindings.
 *)

(*
 * Inductive types
 *)
let bindings_for_inductive env mutind_body ind_bodies : CRD.t list =
  Array.to_list
    (Array.mapi
       (fun i ind_body ->
         let name_id = ind_body.mind_typename in
         let typ = type_of_inductive env i mutind_body in
         CRD.LocalAssum (Name name_id, typ))
       ind_bodies)

(*
 * Fixpoints
 *)
let bindings_for_fix (names : name array) (typs : types array) : CRD.t list =
  Array.to_list
    (CArray.map2_i
       (fun i name typ -> CRD.LocalAssum (name, Vars.lift i typ))
       names typs)
