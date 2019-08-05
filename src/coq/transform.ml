(*
 * Term and type transformation (has side-effects)
 * TODO explain more, maybe move later
 * TODO move out some module stuff etc.
 *)

open Util
open Context
open Environ
open Constr
open Names
open Constrexpr
open Evd
open Utilities
open Declarations
open Decl_kinds
open Constrextern
open Defutils
open Termutils
open Indutils
open Substitution

(* Type-sensitive transformation of terms *)
type constr_transformer = env -> evar_map ref -> constr -> constr

(*
 * Force a constant_body into the internal representation
 * Fail with an error if the supplied constant_body is an axiom
 *)
let force_constant_body const_body =
  match const_body.const_body with
  | Def const_def ->
    Mod_subst.force_constr const_def
  | OpaqueDef opaq ->
    Opaqueproof.force_proof (Global.opaque_tables ()) opaq
  | _ ->
    CErrors.user_err ~hdr:"force_constant_body"
      (Pp.str "An axiom has no defining term")

(*
 * Declare a new constant under the given name with the transformed term and
 * type from the given constant.
 *
 * NOTE: Global side effects.
 *)
let transform_constant ident tr_constr const_body =
  let env =
    match const_body.const_universes with
    | Monomorphic_const univs ->
      Global.env () |> Environ.push_context_set univs
    | Polymorphic_const univs ->
      CErrors.user_err ~hdr:"transform_constant"
        Pp.(str "Universe polymorphism is not supported")
  in
  let term = force_constant_body const_body in
  let evm = ref (Evd.from_env env) in
  let term' = tr_constr env evm term in
  let type' = tr_constr env evm const_body.const_type in
  define_term ~typ:type' ident !evm term' true |> Globnames.destConstRef

(*
 * Declare a new inductive family under the given name with the transformed type
 * arity and constructor types from the given inductive definition. Names for
 * the constructors remain the same.
 *
 * NOTE: Global side effects.
 *)
let transform_inductive ident tr_constr ((mind_body, ind_body) as ind_specif) =
  (* TODO: Can we re-use this for ornamental lifting of inductive families? *)
  let env = Global.env () in
  let env, univs, arity, cons_types =
    open_inductive ~global:true env ind_specif
  in
  let evm = ref (Evd.from_env env) in
  let arity' = tr_constr env evm arity in
  let cons_types' = List.map (tr_constr env evm) cons_types in
  declare_inductive
    ident (Array.to_list ind_body.mind_consnames)
    (is_ind_body_template ind_body) univs
    mind_body.mind_nparams arity' cons_types'
    
(*
 * Pull any functor parameters off the module signature, returning the list of
 * functor parameters and the list of module elements (i.e., fields).
 *)
let decompose_module_signature mod_sign =
  let rec aux mod_arity mod_sign =
    match mod_sign with
    | MoreFunctor (mod_name, mod_type, mod_sign) ->
      aux ((mod_name, mod_type) :: mod_arity) mod_sign
    | NoFunctor mod_fields ->
      mod_arity, mod_fields
  in
  aux [] mod_sign

(*
 * Define an interactive (i.e., elementwise) module structure, with the
 * functional argument called to populate the module elements.
 *
 * The optional argument specifies functor parameters.
 *)
let declare_module_structure ?(params=[]) ident declare_elements =
  let mod_sign = Vernacexpr.Check [] in
  let mod_path =
    Declaremods.start_module Modintern.interp_module_ast None ident params mod_sign
  in
  Dumpglob.dump_moddef mod_path "mod";
  declare_elements ();
  let mod_path = Declaremods.end_module () in
  Dumpglob.dump_modref mod_path "mod";
  Flags.if_verbose Feedback.msg_info
    Pp.(str "\nModule " ++ Id.print ident ++ str " is defined");
  mod_path

      
(*
 * Declare a new module structure under the given name with the compositionally
 * transformed (i.e., forward-substituted) components from the given module
 * structure. Names for the components remain the same.
 *
 * The optional initialization function is called immediately after the module
 * structure begins, and its returned subsitution is applied to all other module
 * elements.
 *
 * NOTE: Does not support functors or nested modules.
 * NOTE: Global side effects.
 *)
let transform_module_structure ?(init=const Globmap.empty) ident tr_constr mod_body =
  let mod_path = mod_body.mod_mp in
  let mod_arity, mod_elems = decompose_module_signature mod_body.mod_type in
  assert (List.is_empty mod_arity); (* Functors are not yet supported *)
  let transform_module_element subst (label, body) =
    let ident = Label.to_id label in
    let tr_constr env evm = subst_globals subst %> tr_constr env evm in
    match body with
    | SFBconst const_body ->
      let const = Constant.make2 mod_path label in
      if Globmap.mem (ConstRef const) subst then
        subst (* Do not transform schematic definitions. *)
      else
        let const' = transform_constant ident tr_constr const_body in
        Globmap.add (ConstRef const) (ConstRef const') subst
    | SFBmind mind_body ->
      check_inductive_supported mind_body;
      let ind = (MutInd.make2 mod_path label, 0) in
      let ind_body = mind_body.mind_packets.(0) in
      let ind' = transform_inductive ident tr_constr (mind_body, ind_body) in
      let ncons = Array.length ind_body.mind_consnames in
      let list_cons ind = List.init ncons (fun i -> ConstructRef (ind, i + 1)) in
      let sorts = ind_body.mind_kelim in
      let list_elim ind = List.map (Indrec.lookup_eliminator ind) sorts in
      Globmap.add (IndRef ind) (IndRef ind') subst |>
      List.fold_right2 Globmap.add (list_cons ind) (list_cons ind') |>
      List.fold_right2 Globmap.add (list_elim ind) (list_elim ind')
    | SFBmodule mod_body ->
      Feedback.msg_warning
        Pp.(str "Skipping nested module structure " ++ Label.print label);
      subst
    | SFBmodtype sig_body ->
      Feedback.msg_warning
        Pp.(str "Skipping nested module signature " ++ Label.print label);
      subst
  in
  declare_module_structure
    ident
    (fun () ->
       ignore (List.fold_left transform_module_element (init ()) mod_elems))
