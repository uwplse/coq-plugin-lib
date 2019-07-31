(*
 * Inductive types and their eliminators (TODO organize, maybe move some stuff)
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
open Envutils
open Termutils
open Typeutils

(* Get the type of an inductive type *)
let type_of_inductive env index mutind_body : types =
  let ind_bodies = mutind_body.mind_packets in
  let ind_body = Array.get ind_bodies index in
  let univs = Declareops.inductive_polymorphic_context mutind_body in
  let univ_instance = Univ.make_abstract_instance univs in
  let mutind_spec = (mutind_body, ind_body) in
  Inductive.type_of_inductive env (mutind_spec, univ_instance)

(*
 * Inductive types create bindings that we need to push to the environment
 * This function gets those bindings
 *)
let bindings_for_inductive env mutind_body ind_bodies : CRD.t list =
  Array.to_list
    (Array.mapi
       (fun i ind_body ->
         let name_id = ind_body.mind_typename in
         let typ = type_of_inductive env i mutind_body in
         CRD.LocalAssum (Name name_id, typ))
       ind_bodies)

(* Don't support mutually inductive or coinductive types yet (TODO move) *)
let check_inductive_supported mutind_body : unit =
  let ind_bodies = mutind_body.mind_packets in
  if Array.length ind_bodies > 1 then
    CErrors.user_err (Pp.str "Mutually inductive types are not supported")
  else if (mutind_body.mind_finite = Declarations.CoFinite) then
    CErrors.user_err (Pp.str "Coinductive types are not supported")

(*
 * Check if a constant is an inductive elminator
 * If so, return the inductive type
 *)
let inductive_of_elim (env : env) (pc : pconstant) : mutual_inductive option =
  let (c, u) = pc in
  let kn = Constant.canonical c in
  let (modpath, dirpath, label) = KerName.repr kn in
  let rec try_find_ind is_rev =
    try
      let label_string = Label.to_string label in
      let label_length = String.length label_string in
      let split_index = String.rindex_from label_string (if is_rev then (label_length - 3) else label_length) '_'  in
      let suffix_length = label_length - split_index in
      let suffix = String.sub label_string split_index suffix_length in
      if (suffix = "_ind" || suffix = "_rect" || suffix = "_rec" || suffix = "_ind_r") then
        let ind_label_string = String.sub label_string 0 split_index in
        let ind_label = Label.of_id (Id.of_string_soft ind_label_string) in
        let ind_name = MutInd.make1 (KerName.make modpath dirpath ind_label) in
        lookup_mind ind_name env;
        Some ind_name
      else
        if not is_rev then
          try_find_ind true
        else
          None
    with _ ->
      if not is_rev then
        try_find_ind true
      else
        None
  in try_find_ind false
                                 
(*
 * Get the number of constructors for an inductive type
 *
 * When we implement mutually inductive types, we may need to
 * update this heuristic.
 *)
let num_constrs (mutind_body : mutual_inductive_body) : int =
  Array.fold_left
    (fun n i ->
      n + (Array.length i.mind_consnames))
    0
    mutind_body.mind_packets

(*
 * Boolean version of above that doesn't care about the term type
 *)
let is_elim (env : env) (trm : types) =
  isConst trm && Option.has_some (inductive_of_elim env (destConst trm))

(* Lookup the eliminator over the type sort *)
let type_eliminator (env : env) (ind : inductive) =
  Universes.constr_of_global (Indrec.lookup_eliminator ind InType)

(* Applications of eliminators *)
type elim_app =
  {
    elim : types;
    pms : types list;
    p : types;
    cs : types list;
    final_args : types list;
  }

(* Apply an eliminator *)
let apply_eliminator (ea : elim_app) : types =
  let args = List.append ea.pms (ea.p :: ea.cs) in
  mkAppl (mkAppl (ea.elim, args), ea.final_args)

(* Deconstruct an eliminator application *)
let deconstruct_eliminator env evd app : elim_app =
  let elim = first_fun app in
  let ip_args = unfold_args app in
  let ip_typ = reduce_type env evd elim in
  let from_i = Option.get (inductive_of_elim env (destConst elim)) in
  let from_m = lookup_mind from_i env in
  let npms = from_m.mind_nparams in
  let from_arity = arity (type_of_inductive env 0 from_m) in
  let num_indices = from_arity - npms in
  let num_props = 1 in
  let num_constrs = arity ip_typ - npms - num_props - num_indices - 1 in
  let (pms, pmd_args) = take_split npms ip_args in
  match pmd_args with
  | p :: cs_and_args ->
     let (cs, final_args) = take_split num_constrs cs_and_args in
     { elim; pms; p; cs; final_args }
  | _ ->
     failwith "can't deconstruct eliminator; no final arguments"

(*
 * Given the type of a case of an eliminator,
 * determine the number of inductive hypotheses
 *)
let rec num_ihs env rec_typ typ =
  match kind typ with
  | Prod (n, t, b) ->
     if is_or_applies rec_typ (reduce_term env t) then
       let (n_b_t, b_t, b_b) = destProd b in
       1 + num_ihs (push_local (n, t) (push_local (n_b_t, b_t) env)) rec_typ b_b
     else
       num_ihs (push_local (n, t) env) rec_typ b
  | _ ->
     0

(* Determine whether template polymorphism is used for a one_inductive_body *)
let is_ind_body_template ind_body =
  match ind_body.mind_arity with
  | RegularArity _ -> false
  | TemplateArity _ -> true

(* Construct the arity of an inductive type from a one_inductive_body *)
let arity_of_ind_body ind_body =
  match ind_body.mind_arity with
  | RegularArity { mind_user_arity; mind_sort } ->
    mind_user_arity
  | TemplateArity { template_param_levels; template_level } ->
    let sort = Constr.mkType template_level in
    recompose_prod_assum ind_body.mind_arity_ctxt sort

(* Create an Entries.local_entry from a Rel.Declaration.t *)
let make_ind_local_entry decl =
  let entry =
    match decl with
    | CRD.LocalAssum (_, typ) -> Entries.LocalAssumEntry typ
    | CRD.LocalDef (_, term, _) -> Entries.LocalDefEntry term
  in
  match CRD.get_name decl with
  | Name.Name id -> (id, entry)
  | Name.Anonymous -> failwith "Parameters to an inductive type may not be anonymous"

(* Instantiate an abstract universe context *)
let inst_abs_univ_ctx abs_univ_ctx =
  (* Note that we're creating *globally* fresh universe levels. *)
  Universes.fresh_instance_from_context abs_univ_ctx |> Univ.UContext.make

(* Instantiate an abstract_inductive_universes into an Entries.inductive_universes with Univ.UContext.t *)
let make_ind_univs_entry = function
  | Monomorphic_ind univ_ctx_set ->
    let univ_ctx = Univ.UContext.empty in
    (Entries.Monomorphic_ind_entry univ_ctx_set, univ_ctx)
  | Polymorphic_ind abs_univ_ctx ->
    let univ_ctx = inst_abs_univ_ctx abs_univ_ctx in
    (Entries.Polymorphic_ind_entry univ_ctx, univ_ctx)
  | Cumulative_ind abs_univ_cumul ->
    let abs_univ_ctx = Univ.ACumulativityInfo.univ_context abs_univ_cumul in
    let univ_ctx = inst_abs_univ_ctx abs_univ_ctx in
    let univ_var = Univ.ACumulativityInfo.variance abs_univ_cumul in
    let univ_cumul = Univ.CumulativityInfo.make (univ_ctx, univ_var) in
    (Entries.Cumulative_ind_entry univ_cumul, univ_ctx)

let open_inductive ?(global=false) env (mind_body, ind_body) =
  let univs, univ_ctx = make_ind_univs_entry mind_body.mind_universes in
  let subst_univs = Vars.subst_instance_constr (Univ.UContext.instance univ_ctx) in
  let env = Environ.push_context univ_ctx env in
  if global then
    Global.push_context false univ_ctx;
  let arity = arity_of_ind_body ind_body in
  let arity_ctx = [CRD.LocalAssum (Name.Anonymous, arity)] in
  let ctors_typ = Array.map (recompose_prod_assum arity_ctx) ind_body.mind_user_lc in
  env, univs, subst_univs arity, Array.map_to_list subst_univs ctors_typ

let declare_inductive typename consnames template univs nparam arity constypes =
  let open Entries in
  let params, arity = Term.decompose_prod_n_assum nparam arity in
  let constypes = List.map (Term.decompose_prod_n_assum (nparam + 1)) constypes in
  let ind_entry =
    { mind_entry_typename = typename;
      mind_entry_arity = arity;
      mind_entry_template = template;
      mind_entry_consnames = consnames;
      mind_entry_lc = List.map snd constypes }
  in
  let mind_entry =
    { mind_entry_record = None;
      mind_entry_finite = Declarations.Finite;
      mind_entry_params = List.map make_ind_local_entry params;
      mind_entry_inds = [ind_entry];
      mind_entry_universes = univs;
      mind_entry_private = None }
  in
  let ((_, ker_name), _) = Declare.declare_mind mind_entry in
  let mind = MutInd.make1 ker_name in
  let ind = (mind, 0) in
  Indschemes.declare_default_schemes mind;
  ind
