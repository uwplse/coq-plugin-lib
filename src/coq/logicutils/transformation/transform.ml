(*
 * Term and type transformation (has side-effects)
 * TODO explain more, maybe move later
 * TODO move out some module stuff etc.
 *)

open Util
open Environ
open Constr
open Names
open Evd
open Declarations
open Defutils
open Indutils
open Substitution
open Stateutils
open Recordops
open Record

(* Type-sensitive transformation of terms *)
type constr_transformer = env -> evar_map -> constr -> evar_map * constr

(*
 * Force a constant_body into the internal representation
 * Fail with an error if the supplied constant_body is an axiom
 *)
let force_constant_body const_body =
  match const_body.const_body with
  | Def const_def ->
    Mod_subst.force_constr const_def
  | OpaqueDef opaq ->
    fst (Opaqueproof.force_proof Library.indirect_accessor (Global.opaque_tables ()) opaq)
  | _ ->
    CErrors.user_err ~hdr:"force_constant_body"
      (Pp.str "An axiom has no defining term")

(*
 * Declare a new constant under the given name with the transformed term and
 * type from the given constant.
 *
 * NOTE: Global side effects.
 * TODO is this the right way to deal w/ env and sigma? in this whole file
 *)
let transform_constant ident tr_constr const_body =
  let env =
    match const_body.const_universes with
    | Monomorphic  univs ->
      Global.env () |> Environ.push_context_set univs
    | Polymorphic univs ->
      CErrors.user_err ~hdr:"transform_constant"
        Pp.(str "Universe polymorphism is not supported")
  in
  let term = force_constant_body const_body in
  let sigma = Evd.from_env env in
  let sigma, term' = tr_constr env sigma term in
  let sigma, type' = tr_constr env sigma const_body.const_type in
  sigma, Globnames.destConstRef (define_term ~typ:type' ident sigma term' true)

(*
 * Declare a new inductive family under the given name with the transformed type
 * arity and constructor types from the given inductive definition. Names for
 * the constructors remain the same.
 *
 * NOTE: Global side effects.
 *)
let transform_inductive ident tr_constr (mind_body, ind_body as ind_specif) =
  (* TODO: Can we re-use this for ornamental lifting of inductive families? *)
  let env = Global.env () in
  let env, univs, arity, cons_types =
    open_inductive ~global:true env ind_specif
  in
  let sigma = Evd.from_env env in
  let sigma, arity' = tr_constr env sigma arity in
  let sigma, cons_types' = map_state (fun tr sigma -> tr_constr env sigma tr) cons_types sigma in
  Util.on_snd
    (declare_inductive
       ident
       (Array.to_list ind_body.mind_consnames)
       (is_ind_body_template ind_body)
       univs
       mind_body.mind_nparams
       arity')
    (sigma, cons_types')

(*
 * Try to register a transformed inductive type inside a module as a record,
 * if appropriate
 *)
let try_register_record mod_path (ind, ind') =
  try
    let r = lookup_structure ind in
    Feedback.msg_info (Pp.str "Transformed a record");
    let pks = r.s_PROJKIND in
    let ps =
      List.map
        (Option.map (fun p -> Constant.make2 mod_path (Constant.label p)))
        r.s_PROJ
    in
    (try
       declare_structure (ind', (ind', 1), pks, ps)
     with _ ->
       Feedback.msg_warning
         (Pp.str "Failed to register projections for transformed record"))
  with Not_found ->
    ()

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
 *
 * TODO sigma handling, not sure how to do it here/if we need it
 *)
let transform_module_structure ?(init=const Globnames.Refmap.empty) ?(opaques=Globnames.Refset.empty) ident tr_constr mod_body =
  let open Modutils in
  let mod_path = mod_body.mod_mp in
  let mod_arity, mod_elems = decompose_module_signature mod_body.mod_type in
  let mod_elems =
    List.filter
      (fun (l, b) ->
        match b with
        | SFBconst const_body ->
           let const = Constant.make2 mod_path l in
           not (Globnames.Refset.mem (ConstRef const) opaques)
        | _ ->
           true)
      mod_elems
  in
  assert (List.is_empty mod_arity); (* Functors are not yet supported *)
  let transform_module_element mod_path' subst (label, body) =
    let open GlobRef in
    Feedback.msg_info (Pp.(str "Transforming " ++ Label.print label));
    let ident = Label.to_id label in
    let tr_constr env sigma = subst_globals subst %> tr_constr env sigma in
    match body with
    | SFBconst const_body ->
      let const = Constant.make2 mod_path label in
      if Globnames.Refmap.mem (ConstRef const) subst then
        subst (* Do not transform schematic definitions. *)
      else
        let sigma, const' = transform_constant ident tr_constr const_body in
        Globnames.Refmap.add (ConstRef const) (ConstRef const') subst
    | SFBmind mind_body ->
      check_inductive_supported mind_body;
      let ind = (MutInd.make2 mod_path label, 0) in
      let ind_body = mind_body.mind_packets.(0) in
      let sigma, ind' = transform_inductive ident tr_constr (mind_body, ind_body) in
      try_register_record mod_path' (ind, ind');
      let ncons = Array.length ind_body.mind_consnames in
      let list_cons ind = List.init ncons (fun i -> ConstructRef (ind, i + 1)) in
      let sorts = ind_body.mind_kelim in
      let list_elim ind = List.map (Indrec.lookup_eliminator ind) sorts in
      Globnames.Refmap.add (IndRef ind) (IndRef ind') subst |>
      List.fold_right2 Globnames.Refmap.add (list_cons ind) (list_cons ind') |>
      List.fold_right2 Globnames.Refmap.add (list_elim ind) (list_elim ind')
    | SFBmodule mod_body ->
      Feedback.msg_warning (Pp.str "Skipping nested module structure");
      subst
    | SFBmodtype sig_body ->
      Feedback.msg_warning (Pp.str "Skipping nested module signature");
      subst
  in
  declare_module_structure
    ident
    (fun mod_path' ->
       ignore (List.fold_left (transform_module_element mod_path') (init ()) mod_elems))
