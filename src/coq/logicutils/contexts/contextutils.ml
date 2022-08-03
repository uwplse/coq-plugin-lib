(*
 * Utilities for contexts
 *
 * Many of these are by Nate Yazdani from the original DEVOID code.
 * Some are not. Just check the Git history of DEVOID if you are interested.
 *)

open Constr
open Names
open Environ
open Inference
open Declarations

module CRD = Context.Rel.Declaration
module CND = Context.Named.Declaration
                       
(* --- Deconstructing declarations --- *)

(* Get the name of a rel declaration *)
let rel_name decl = CRD.get_name decl

(* Get the optional value of a rel declaration *)
let rel_value decl = CRD.get_value decl

(* Get the type of a rel declaration *)
let rel_type decl = CRD.get_type decl

(* Get the identifier of a named declaration *)
let named_ident decl = CND.get_id decl

(* Get the optional value of a named declaration *)
let named_value decl = CND.get_value decl

(* Get the type of a named declaration *)
let named_type decl = CND.get_type decl

(* --- Questions about declarations --- *)

(* Is the rel declaration a local assumption? *)
let is_rel_assum = CRD.is_local_assum

(* Is the rel declaration a local definition? *)
(* let is_rel_defin x = CRD.is_local_def (get_rel_ctx_name x) *)
let is_rel_defin x = CRD.is_local_def x

(* Is the named declaration an assumption? *)
let is_named_assum = CND.is_local_assum

(* Is the named declaration a definition? *)
let is_named_defin = CND.is_local_def
    
(* --- Context manipulation tools --- *)

(* Get relative context for a name *)
let get_rel_ctx_name name = 
  match name with (* handle if anon or not *)
  | Anonymous -> Context.anonR
  | Name idt -> Context.nameR idt

(* Get relative context for a declaration *)
let get_rel_ctx decl = get_rel_ctx_name (rel_name decl)

(* --- Constructing declarations --- *)

(* Make the rel declaration for a local assumption *)
let rel_assum (name, typ) = CRD.LocalAssum (get_rel_ctx_name name, typ)

(* Make the rel declaration for a local definition *)
let rel_defin (name, def, typ) = CRD.LocalDef (get_rel_ctx_name name, def, typ)

(* Make the named declaration for a local assumption *)
let named_assum (id, typ) = CND.LocalAssum (id, typ)

(* Make the named declaration for a local definition *)
let named_defin (id, def, typ) = CND.LocalDef (id, def, typ)

(*
 * Instantiate a local assumption as a local definition, using the provided term
 * as its definition.
 *
 * Raises an assertion error if the local declaration is not a local assumption.
 * TODO raise (and catch) a better error here
 *)
let define_rel_decl body decl =
  assert (is_rel_assum decl);
  rel_defin (rel_name decl, body, rel_type decl)

(* --- Mapping over contexts --- *)

(* Map over a rel context with environment kept in sync *)
let map_rel_context env make ctxt =
  Context.Rel.fold_outside
    (fun decl (env, res) ->
       push_rel decl env, (make env decl) :: res)
    ctxt
    ~init:(env, []) |>
    snd

(* Map over a named context with environment kept in synch *)
let map_named_context env make ctxt =
  Context.Named.fold_outside
    (fun decl (env, res) ->
       push_named decl env, (make env decl) :: res)
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
       | None -> mkProd (get_rel_ctx decl, rel_type decl, body))
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
       | None -> mkLambda (get_rel_ctx decl, rel_type decl, body))
    ~init:body
    ctxt

(*
 * Decompose the first n product bindings, zeta-reducing let bindings to reveal
 * further product bindings when necessary.
 *)
let decompose_prod_n_zeta n term =
  assert (n >= 0);
  let open Context in
  let rec aux n ctxt body =
    if n > 0 then
      match Constr.kind body with
      | Prod (name, param, body) ->
        aux (n - 1) (Rel.add (rel_assum (name.binder_name, param)) ctxt) body
      | LetIn (name, def_term, def_type, body) ->
        aux n ctxt (Vars.subst1 def_term body)
      | _ ->
        invalid_arg "decompose_prod_n_zeta: not enough products"
    else
      ctxt, body
  in
  aux n Rel.empty term

(*
 * Decompose the first n lambda bindings, zeta-reducing let bindings to reveal
 * further lambda bindings when necessary.
 *)
let decompose_lam_n_zeta n term =
  assert (n >= 0);
  let open Context in
  let rec aux n ctxt body =
    if n > 0 then
      match Constr.kind body with
      | Lambda (name, param, body) ->
        aux (n - 1) (Rel.add (rel_assum (name.binder_name, param)) ctxt) body
      | LetIn (name, def_term, def_type, body) ->
        Vars.subst1 def_term body |> aux n ctxt
      | _ ->
        invalid_arg "decompose_lam_n_zeta: not enough lambdas"
    else
      ctxt, body
  in
  aux n Rel.empty term

(* Bind the declarations of a local context as product/let-in bindings *)
let recompose_prod_assum decls term =
  let bind term decl = Term.mkProd_or_LetIn decl term in
  Context.Rel.fold_inside bind ~init:term decls

(* Bind the declarations of a local context as lambda/let-in bindings *)
let recompose_lam_assum decls term =
  let bind term decl = Term.mkLambda_or_LetIn decl term in
  Context.Rel.fold_inside bind ~init:term decls

(* --- Names in contexts --- *)

(*
 * Give a "reasonable" name to each anonymous local declaration in the relative
 * context. Name generation is according to standard Coq policy (cf., Namegen)
 * and does not guarantee freshness, but term type-checking is only sensitive to
 * anonymity. (Names are freshened by subscription when printed.)
 *)
let deanonymize_context env sigma ctxt =
  List.map EConstr.of_rel_decl ctxt |>
  Namegen.name_context env sigma |>
  List.map (EConstr.to_rel_decl sigma)

(* --- Getting bindings for certain kinds of terms --- *)

(*
 * Certain terms create bindings you need to push to the environment as you
 * recurse. These functions give you those bindings.
 *)

(*
 * Inductive types
 *)
let bindings_for_inductive env mutind_body ind_bodies : rel_declaration list =
  Array.to_list
    (Array.mapi
       (fun i ind_body ->
         let name_id = ind_body.mind_typename in
         let typ = type_of_inductive env i mutind_body in
         CRD.LocalAssum (Context.nameR name_id, typ))
       mutind_body.mind_packets)

(*
 * Fixpoints
 *)
let bindings_for_fix (names : name array) (typs : types array) : rel_declaration list =
  Array.to_list
    (CArray.map2_i
       (fun i name typ -> CRD.LocalAssum (name, Vars.lift i typ))
       (Array.map get_rel_ctx_name names)
       typs)

(* --- Combining contexts --- *)

(* 
 * Append two contexts (inner first, outer second), shifting internal indices. 
 *)
let context_app inner outer =
  List.append
    (Termops.lift_rel_context (Context.Rel.length outer) inner)
    outer
