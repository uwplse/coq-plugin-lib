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
open Printing

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
  let open Printing in
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
    (try
       declare_structure_entry (r.s_CONST, r.s_PROJKIND, r.s_PROJ)
     with _ ->
       Feedback.msg_warning
         (Pp.str "Failed to register projections for transformed record"))
  with Not_found ->
    ()

let lookup_eliminator_error_handling ind sorts = 
  let env = Global.env () in
  List.filter_map (fun x -> x)
  (List.map 
    (fun x -> 
      Feedback.msg_warning (Sorts.pr_sort_family (x));
      try Some (Indrec.lookup_eliminator env ind x)
      with
      | _ -> None
    )
    sorts)

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
let transform_module_structure env ?(init=const GlobRef.Map.empty) ?(opaques=GlobRef.Set.empty) ident tr_constr mod_body =
  let open Modutils in
  let mod_path = mod_body.mod_mp in
  let mod_arity, mod_elems = decompose_module_signature mod_body.mod_type in
  let mod_elems =
    List.filter
      (fun (l, b) ->
        match b with
        | SFBconst const_body ->
           let const = Constant.make2 mod_path l in
           not (GlobRef.Set.mem (ConstRef const) opaques)
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
      if GlobRef.Map.mem (ConstRef const) subst then
        subst (* Do not transform schematic definitions. *)
      else
        let sigma, const' = transform_constant ident tr_constr const_body in
        GlobRef.Map.add (ConstRef const) (ConstRef const') subst
    | SFBmind mind_body ->
      check_inductive_supported mind_body;
      let ind = (MutInd.make2 mod_path label, 0) in
      let ind_body = mind_body.mind_packets.(0) in
      let sigma, ind' = transform_inductive ident tr_constr (mind_body, ind_body) in
      try_register_record mod_path' (ind, ind');
      let subst = GlobRef.Map.add (IndRef ind) (IndRef ind') subst in
      let ncons = Array.length ind_body.mind_consnames in
      let list_cons ind = List.init ncons (fun i -> ConstructRef (ind, i + 1)) in
      (* let sorts = List.map Sorts.family [Sorts.sprop; Sorts.set; Sorts.prop; Sorts.type1] in *)
      let sorts = List.map Sorts.family [Sorts.sprop; Sorts.prop; Sorts.set; Sorts.type1] in
      let list_elim ind_arg = lookup_eliminator_error_handling ind_arg sorts in
      Feedback.msg_warning (Names.MutInd.print (fst ind));
      Feedback.msg_warning (Pp.str ("ind level " ^ (string_of_int (snd ind))));
      Feedback.msg_warning (Names.MutInd.print (fst ind'));
      Feedback.msg_warning (Pp.str ("ind level " ^ (string_of_int (snd ind'))));
      Feedback.msg_warning (Pp.str (string_of_int (List.length (list_cons ind))));
      Feedback.msg_warning (Pp.str (string_of_int (List.length (list_elim ind))));
      Feedback.msg_warning (Pp.str (string_of_int (List.length (list_cons ind'))));
      Feedback.msg_warning (Pp.str (string_of_int (List.length (list_elim ind'))));
      GlobRef.Map.add (IndRef ind) (IndRef ind') subst |>
      List.fold_right2 GlobRef.Map.add (list_cons ind) (list_cons ind') |>
      List.fold_right2 GlobRef.Map.add (list_elim ind) (list_elim ind')
      (* let sorts = List.map (fun x -> x ind_body.mind_kelim) sort_funcs in *)
      (* let sorts = List.filter (fun x -> match Sorts.relevance_of_sort_family x with 
                                        | Sorts.Relevant -> true
                                        | Sorts.Irrelevant -> false 
                              )  *)
      (* List.filter (fun x -> Sorts.family_leq x ind_body.mind_kelim) *)
      (* Feedback.msg_warning (Pp.str (string_of_int (List.length sorts))); *)
      (*
      let subst = GlobRef.Map.add (IndRef ind) (IndRef ind') subst in 
      let subst = List.fold_right2 GlobRef.Map.add (list_cons ind) (list_cons ind') subst in
      let subst = List.fold_right2 GlobRef.Map.add (list_elim ind) (list_elim ind') subst in 
      subst *)
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
