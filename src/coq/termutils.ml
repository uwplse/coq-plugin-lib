(*
 * Coq term and environment management
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

module Globmap = Globnames.Refmap
module Globset = Globnames.Refset

module CRD = Context.Rel.Declaration (* TODO remove eventually *)

(*
 * Note: This will clean up significantly when we merge DEVOID and PUMPKIN,
 * and split back into multiple files. We'll also use better evar map and
 * universe hygiene at that point.
 *)

(* --- Auxiliary types --- *)
               
type closure = env * (types list)
               
(* --- Constants --- *)

let coq_init_specif =
  ModPath.MPfile
    (DirPath.make (List.map Id.of_string ["Specif"; "Init"; "Coq"]))
                       
let coq_init_logic =
  ModPath.MPfile
    (DirPath.make (List.map Id.of_string ["Logic"; "Init"; "Coq"]))

let coq_init_datatypes =
  ModPath.MPfile
    (DirPath.make (List.map Id.of_string ["Datatypes"; "Init"; "Coq"]))

(* sigma types *)
let sigT : types =
  mkInd (MutInd.make1 (KerName.make2 coq_init_specif (Label.make "sigT")), 0)

(* Introduction for sigma types *)
let existT : types =
  mkConstruct (fst (destInd sigT), 1)

(* Elimination for sigma types *)
let sigT_rect : types =
  mkConst (Constant.make2 coq_init_specif (Label.make "sigT_rect"))

(* Left projection *)
let projT1 : types =
  mkConst (Constant.make2 coq_init_specif (Label.make "projT1"))

(* Right projection *)
let projT2 : types =
  mkConst (Constant.make2 coq_init_specif (Label.make "projT2"))

(* equality *)
let eq : types =
  mkInd (MutInd.make1 (KerName.make2 coq_init_logic (Label.make "eq")), 0)

(* Constructor for quality *)
let eq_refl : types =
  mkConstruct (fst (destInd eq), 1)
        
(* Symmetric eliminator for equality *)
let eq_ind_r : types =
  mkConst (Constant.make2 coq_init_logic (Label.make "eq_ind_r"))

(* Eliminator for equality *)
let eq_ind : types =
  mkConst (Constant.make2 coq_init_logic (Label.make "eq_ind"))

(* Symmetric eleiminator for equality into type *)
let eq_rec_r : types =
  mkConst (Constant.make2 coq_init_logic (Label.make "eq_rec_r"))

(* Eliminator for equality into type *)
let eq_rec : types =
  mkConst (Constant.make2 coq_init_logic (Label.make "eq_rec"))

(* Symmetry *)
let eq_sym : types =
  mkConst (Constant.make2 coq_init_logic (Label.make "eq_sym"))

(* The identity proposition *)
let id_prop : types =
  mkConst (Constant.make2 coq_init_datatypes (Label.make "idProp"))

(* The identity type *)
let id_typ : types =
  mkConst (Constant.make2 coq_init_datatypes (Label.make "id"))

(* --- Questions about constants --- *)

(* Determine if a term applies an identity term *)
let applies_identity (trm : types) : bool =
  match kind trm with
  | App (f, _) ->
     equal f id_prop || equal f id_typ
  | _ ->
     false

(*
 * Check if a term is a rewrite via eq_ind or eq_ind_r
 * For efficiency, just check syntactic equality
 * Don't consider convertible terms for now
 *)
let is_rewrite (trm : types) : bool =
  let eq_term = equal trm in
  eq_term eq_ind_r || eq_term eq_ind || eq_term eq_rec_r || eq_term eq_rec

(* --- Convenient applications of constants --- *)
                                                
(* Get the Coq identity term for typ *)
let identity_term (env : env) (typ : types) : types =
  let id = mkApp (id_prop, Array.make 1 typ) in
  try
    let _ = Typeops.infer env id in id
  with _ -> mkApp (id_typ, Array.make 1 typ)
                                                                    
(* --- Representations --- *)

(** Construct the external expression for a definition. *)
let expr_of_global (g : global_reference) : constr_expr =
  let r = extern_reference Id.Set.empty g in
  CAst.make @@ (CAppExpl ((None, r, None), []))

(* Intern a term (for now, ignore the resulting evar_map) *)
let intern env evd t : types =
  let (trm, _) = Constrintern.interp_constr env evd t in
  EConstr.to_constr evd trm

(* Extern a term *)
let extern env evd t : constr_expr =
  Constrextern.extern_constr true env evd (EConstr.of_constr t)

(* https://github.com/ybertot/plugin_tutorials/blob/master/tuto1/src/simple_declare.ml *)
let edeclare ident (_, poly, _ as k) ~opaque sigma udecl body tyopt imps hook refresh =
  let open EConstr in
  (* XXX: "Standard" term construction combinators such as `mkApp`
     don't add any universe constraints that may be needed later for
     the kernel to check that the term is correct.

     We could manually call `Evd.add_universe_constraints`
     [high-level] or `Evd.add_constraints` [low-level]; however, that
     turns out to be a bit heavyweight.

     Instead, we call type inference on the manually-built term which
     will happily infer the constraint for us, even if that's way more
     costly in term of CPU cycles.

     Beware that `type_of` will perform full type inference including
     canonical structure resolution and what not.
   *)
  let env = Global.env () in
  let sigma =
    if refresh then
      fst (Typing.type_of ~refresh:false env sigma body)
    else
      sigma
  in
  let sigma = Evd.minimize_universes sigma in
  let body = to_constr sigma body in
  let tyopt = Option.map (to_constr sigma) tyopt in
  let uvars_fold uvars c =
    Univ.LSet.union uvars (Univops.universes_of_constr env c) in
  let uvars = List.fold_left uvars_fold Univ.LSet.empty
    (Option.List.cons tyopt [body]) in
  let sigma = Evd.restrict_universe_context sigma uvars in
  let univs = Evd.check_univ_decl ~poly sigma udecl in
  let ubinders = Evd.universe_binders sigma in
  let ce = Declare.definition_entry ?types:tyopt ~univs body in
  DeclareDef.declare_definition ident k ce ubinders imps hook

(* Define a new Coq term *)
let define_term ?typ (n : Id.t) (evm : evar_map) (trm : types) (refresh : bool) =
  let k = (Global, Flags.is_universe_polymorphism(), Definition) in
  let udecl = Univdecls.default_univ_decl in
  let nohook = Lemmas.mk_hook (fun _ x -> x) in
  let etrm = EConstr.of_constr trm in
  let etyp = Option.map EConstr.of_constr typ in
  edeclare n k ~opaque:false evm udecl etrm etyp [] nohook refresh

(* Safely extract the body of a constant, instantiating any universe variables. *)
let open_constant env const =
  let (Some (term, auctx)) = Global.body_of_constant const in
  let uctx = Universes.fresh_instance_from_context auctx |> Univ.UContext.make in
  let term = Vars.subst_instance_constr (Univ.UContext.instance uctx) term in
  let env = Environ.push_context uctx env in
  env, term


(* --- Application and arguments --- *)

(* Get a list of all arguments, fully unfolded at the head *)
let unfold_args_app trm =
  let (f, args) = destApp trm in
  let rec unfold trm =
    match kind trm with
    | App (f, args) ->
       List.append (unfold f) (Array.to_list args)
    | _ ->
       [trm]
  in List.append (List.tl (unfold f)) (Array.to_list args)

(* Like unfold_args_app, but return empty if it's not an application *)
let unfold_args trm =
  if isApp trm then unfold_args_app trm else []

(* Get the last argument of an application *)
let last_arg trm =
  if isApp trm then last (unfold_args trm) else failwith "not an application"

(* Get the first function of an application *)
let rec first_fun t =
  match kind t with
  | App (f, args) ->
     first_fun f
  | _ ->
     t

(*
 * Get the argument to an application of a property at argument position i
 * This unfolds all arguments first
 *)
let get_arg i trm =
  match kind trm with
  | App (_, _) ->
     let args = Array.of_list (unfold_args trm) in
     Array.get args i
  | _ ->
     failwith "not an application"

(* --- Constructing terms --- *)

(* mkApp with a list *)
let mkAppl (f, args) = mkApp (f, Array.of_list args)

(* Define a constant from an ID in the current path *)
let make_constant id =
  mkConst (Constant.make1 (Lib.make_kn id))
                             
(* Recursively turn a product into a function *)
let rec prod_to_lambda trm =
  match kind trm with
  | Prod (n, t, b) ->
     mkLambda (n, t, prod_to_lambda b)
  | _ ->
     trm

(* Recursively turn a function into a product *)
let rec lambda_to_prod trm =
  match kind trm with
  | Lambda (n, t, b) ->
     mkProd (n, t, lambda_to_prod b)
  | _ ->
     trm
      
(*
 * An application of eq
 *)
type eq_app =
  {
    at_type : types;
    trm1 : types;
    trm2 : types;
  }

(*
 * Make an eq type
 *)
let apply_eq (app : eq_app) : types =
  mkAppl (eq, [app.at_type; app.trm1; app.trm2])

(*
 * Deconstruct an eq type
 *)
let dest_eq (trm : types) : eq_app =
  let [at_type; trm1; trm2] = unfold_args trm in
  { at_type; trm1; trm2 }

(*
 * An application of eq_sym
 *)
type eq_sym_app =
  {
    eq_typ : eq_app;
    eq_proof : types;
  }

(*
 * Make an eq type
 *)
let apply_eq_sym (app : eq_sym_app) : types =
  let eq_typ = app.eq_typ in
  mkAppl (eq_sym, [eq_typ.at_type; eq_typ.trm1; eq_typ.trm2; app.eq_proof])

(*
 * Deconstruct an eq type
 *)
let dest_eq_sym (trm : types) : eq_sym_app =
  let [at_type; trm1; trm2; eq_proof] = unfold_args trm in
  let eq_typ = { at_type; trm1; trm2 } in
  { eq_typ; eq_proof }
    
(*
 * An application of eq_ind
 *)
type eq_ind_app =
  {
    at_type : types;
    p : types;
    trm1 : types;
    trm2 : types;
    h : types;
    b : types;
  }

(*
 * Apply an eq_ind
 *)
let apply_eq_ind (app : eq_ind_app) : types =
  mkAppl (eq_ind, [app.at_type; app.trm1; app.p; app.b; app.trm2; app.h])

(* 
 * Deconstruct an eq_ind
 *)
let dest_eq_ind (trm : types) : eq_ind_app =
  let [at_type; trm1; p; b; trm2; h] = unfold_args trm in
  { at_type; trm1; p; b; trm2; h }

(*
 * An application of eq_refl
 *)
type eq_refl_app =
  {
    typ : types;
    trm : types;
  }

(*
 * Apply an eq_refl
 *)
let apply_eq_refl (app : eq_refl_app) : types =
  mkAppl (eq_refl, [app.typ; app.trm])

(* 
 * Deconstruct an eq_refl
 *)
let dest_eq_refl (trm : types) : eq_refl_app =
  let [typ; trm] = unfold_args trm in
  { typ; trm }

(*
 * An application of existT
 *)
type existT_app =
  {
    index_type : types;
    packer : types;
    index : types;
    unpacked : types;
  }

(*
 * Pack an existT term from an index type, packer, index, and unpacked version
 *)
let pack_existT (app : existT_app) : types =
  mkAppl (existT, [app.index_type; app.packer; app.index; app.unpacked])

(*
 * Deconstruct an existT term
 *)
let dest_existT (trm : types) : existT_app =
  let [index_type; packer; index; unpacked] = unfold_args trm in
  { index_type; packer; index; unpacked }

(*
 * An application of sigT
 *)
type sigT_app =
  {
    index_type : types;
    packer : types;
  }

(*
 * Pack a sigT type from an index type and a packer
 *)
let pack_sigT (app : sigT_app) =
  mkAppl (sigT, [app.index_type; app.packer])

(*
 * Deconsruct a sigT type from a type
 *)
let dest_sigT (typ : types) =
  let [index_type; packer] = unfold_args typ in
  { index_type; packer }

(*
 * Build the eta-expansion of a term known to have a sigma type.
 *)
let eta_sigT (term : constr) (typ : types) =
  let { index_type; packer } = dest_sigT typ in
  let fst = mkApp (projT1, [|index_type; packer; term|]) in
  let snd = mkApp (projT2, [|index_type; packer; term|]) in
  mkApp (existT, [|index_type; packer; fst; snd|])

(*
 * An application of sigT_rect
 *)
type sigT_elim =
  {
    to_elim : sigT_app;
    packed_type : types;
    unpacked : types;
    arg : types;
  }

(*
 * Eliminate a sigT given an index type, packer, packed type, unpacked term,
 * and the term itself
 *)
let elim_sigT (app : sigT_elim) =
  let index_type = app.to_elim.index_type in
  let packer = app.to_elim.packer in
  let packed_type = app.packed_type in
  let unpacked = app.unpacked in
  let arg = app.arg in
  mkAppl (sigT_rect, [index_type; packer; packed_type; unpacked; arg])

(*
 * Deconstruct an application of sigT_rect
 *)
let dest_sigT_elim (trm : types) =
  let [index_type; packer; packed_type; unpacked; arg] = unfold_args trm in
  let to_elim = { index_type ; packer } in
  { to_elim; packed_type; unpacked; arg }

(*
 * Left projection of a sigma type
 *)
let project_index (app : sigT_app) trm =
  mkAppl (projT1, [app.index_type; app.packer; trm])

(*
 * Right projection of a sigma type
 *)
let project_value (app : sigT_app) trm =
  mkAppl (projT2, [app.index_type; app.packer; trm])

(*
 * Both projections of a sigma type
 *)
let projections (app : sigT_app) trm =
  (project_index app trm, project_value app trm)

(* --- Convertibility and reduction --- *)

(* Safely instantiate a global reference, with proper universe handling (TODO move) *)
let e_new_global evm gref =
  Evarutil.e_new_global evm gref |> EConstr.to_constr !evm

(* Default reducer *)
let reduce_term (env : env) (trm : types) : types =
  EConstr.to_constr
    Evd.empty
    (Reductionops.nf_betaiotazeta env Evd.empty (EConstr.of_constr trm))

(* Delta reduction *)
let delta (env : env) (trm : types) =
  EConstr.to_constr
    Evd.empty
    (Reductionops.whd_delta env Evd.empty (EConstr.of_constr trm))

(* Weak head reduction *)
let whd (env : env) (sigma : evar_map) (trm : types) : types =
  EConstr.to_constr
    sigma
    (Reductionops.whd_all env sigma (EConstr.of_constr trm))

(*
 * There's a part of the env that has opacity info,
 * so if you want to make some things opaque, can add them
 * get env, store it, call set_strategy w/ opaque,
 * then revert later
 *
 * See environ.mli
 * set_oracle
 * set_strategy
 *)

(* nf_all *)
let reduce_nf (env : env) (trm : types) : types =
  EConstr.to_constr
    Evd.empty
    (Reductionops.nf_all env Evd.empty (EConstr.of_constr trm))

(* Chain reduction *)
let chain_reduce rg rf (env : env) (trm : types) : types =
  rg env (rf env trm)

(* --- Environments (TODO rename) --- *)

(* Is the rel declaration a local assumption? *)
let is_rel_assum = Rel.Declaration.is_local_assum

(* Is the rel declaration a local definition? *)
let is_rel_defin = Rel.Declaration.is_local_def

(* Make the rel declaration for a local assumption *)
let rel_assum (name, typ) =
  Rel.Declaration.LocalAssum (name, typ)

(* Make the rel declaration for a local definition *)
let rel_defin (name, def, typ) =
  Rel.Declaration.LocalDef (name, def, typ)

(* Get the name of a rel declaration *)
let rel_name decl =
  Rel.Declaration.get_name decl

(* Get the optional value of a rel declaration *)
let rel_value decl =
  Rel.Declaration.get_value decl

(* Get the type of a rel declaration *)
let rel_type decl =
  Rel.Declaration.get_type decl

(* Map over a rel context with environment kept in synch *)
let map_rel_context env make ctxt =
  Rel.fold_outside
    (fun decl (env, res) ->
       push_rel decl env, (make env decl) :: res)
    ctxt
    ~init:(env, []) |>
  snd

(*
 * Bind all local declarations in the relative context onto the body term as
 * products, substituting away (i.e., zeta-reducing) any local definitions.
 *)
let smash_prod_assum ctxt body =
  Rel.fold_inside
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
  Rel.fold_inside
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
        aux (n - 1) (Rel.add (rel_assum (name, param)) ctxt) body
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
  let rec aux n ctxt body =
    if n > 0 then
      match Constr.kind body with
      | Lambda (name, param, body) ->
        aux (n - 1) (Rel.add (rel_assum (name, param)) ctxt) body
      | LetIn (name, def_term, def_type, body) ->
        Vars.subst1 def_term body |> aux n ctxt
      | _ ->
        invalid_arg "decompose_lam_n_zeta: not enough lambdas"
    else
      ctxt, body
  in
  aux n Rel.empty term

(* Is the named declaration an assumption? *)
let is_named_assum = Named.Declaration.is_local_assum

(* Is the named declaration a definition? *)
let is_named_defin = Named.Declaration.is_local_def

(* Make the named declaration for a local assumption *)
let named_assum (id, typ) =
  Named.Declaration.LocalAssum (id, typ)

(* Make the named declaration for a local definition *)
let named_defin (id, def, typ) =
  Named.Declaration.LocalDef (id, def, typ)

(* Get the name of a named declaration *)
let named_ident decl =
  Named.Declaration.get_id decl

(* Get the optional value of a named declaration *)
let named_value decl =
  Named.Declaration.get_value decl

(* Get the type of a named declaration *)
let named_type decl =
  Named.Declaration.get_type decl

(* Map over a named context with environment kept in synch *)
let map_named_context env make ctxt =
  Named.fold_outside
    (fun decl (env, res) ->
       push_named decl env, (make env decl) :: res)
    ctxt
    ~init:(env, []) |>
  snd

(*
 * Similarly but for fixpoints
 *)
let bindings_for_fix (names : name array) (typs : types array) : CRD.t list =
  Array.to_list
    (CArray.map2_i
       (fun i name typ -> CRD.LocalAssum (name, Vars.lift i typ))
       names typs)

(* Append two contexts (inner first, outer second), shifting internal indices. *)
let context_app inner outer =
  List.append
    (Termops.lift_rel_context (Rel.length outer) inner)
    outer

(* Bind the declarations of a local context as product/let-in bindings *)
let recompose_prod_assum decls term =
  let bind term decl = Term.mkProd_or_LetIn decl term in
  Rel.fold_inside bind ~init:term decls

(* Bind the declarations of a local context as lambda/let-in bindings *)
let recompose_lam_assum decls term =
  let bind term decl = Term.mkLambda_or_LetIn decl term in
  Rel.fold_inside bind ~init:term decls

(* --- Basic questions about terms --- *)

(*
 * Get the arity of a function or function type
 *)
let rec arity p =
  match kind p with
  | Lambda (_, _, b) ->
     1 + arity b
  | Prod (_, _, b) ->
     1 + arity b
  | _ ->
     0

(* Check whether trm applies f (using equal for equality) *)
let applies (f : types) (trm : types) =
  match kind trm with
  | App (g, _) ->
     equal f g
  | _ ->
     false

(* Check whether trm is trm' or applies trm', using equal *)
let is_or_applies (trm' : types) (trm : types) : bool =
  applies trm' trm || equal trm' trm

(* Is the first term equal to a "head" (application prefix) of the second?
 * The notion of term equality is syntactic (i.e., no environment) and defaults
 * to syntactic equality modulo alpha, casts, grouping, and universes. The
 * result of this function is an informative boolean: an optional array, with
 * None meaning false and Some meaning true and giving the trailing arguments.
 *
 * This function is similar to is_or_applies, except for term equality and the
 * informative boolean result.
 *)
let eq_constr_head ?(eq_constr=eq_constr_nounivs) term term' =
  let head, args = decompose_app term in
  let head', args' = decompose_app term' in
  if eq_constr head head' && List.prefix_of eq_constr args args' then
    Some (List.skipn (List.length args) args' |> Array.of_list)
  else
    None

(* --- Basic mapping --- *)

(*
 * Recurse on a mapping function with an environment for a fixpoint
 *)
let map_rec_env_fix map_rec d env a (ns : Name.t array) (ts : types array) =
  let fix_bindings = bindings_for_fix ns ts in
  let env_fix = push_rel_context fix_bindings env in
  let n = List.length fix_bindings in
  let d_n = List.fold_left (fun a' _ -> d a') a (range 0 n) in
  map_rec env_fix d_n

(*
 * Map a function over a term in an environment
 * Update the environment as you go
 * Update the argument of type 'a using the a supplied update function
 * Return a new term
 *)
let rec map_term_env f d (env : env) (a : 'a) (trm : types) : types =
  let map_rec = map_term_env f d in
  match kind trm with
  | Cast (c, k, t) ->
     let c' = map_rec env a c in
     let t' = map_rec env a t in
     mkCast (c', k, t')
  | Prod (n, t, b) ->
     let t' = map_rec env a t in
     let b' = map_rec (push_local (n, t) env) (d a) b in
     mkProd (n, t', b')
  | Lambda (n, t, b) ->
     let t' = map_rec env a t in
     let b' = map_rec (push_local (n, t) env) (d a) b in
     mkLambda (n, t', b')
  | LetIn (n, trm, typ, e) ->
     let trm' = map_rec env a trm in
     let typ' = map_rec env a typ in
     let e' = map_rec (push_let_in (n, e, typ) env) (d a) e in
     mkLetIn (n, trm', typ', e')
  | App (fu, args) ->
     let fu' = map_rec env a fu in
     let args' = Array.map (map_rec env a) args in
     mkApp (fu', args')
  | Case (ci, ct, m, bs) ->
     let ct' = map_rec env a ct in
     let m' = map_rec env a m in
     let bs' = Array.map (map_rec env a) bs in
     mkCase (ci, ct', m', bs')
  | Fix ((is, i), (ns, ts, ds)) ->
     let ts' = Array.map (map_rec env a) ts in
     let ds' = Array.map (map_rec_env_fix map_rec d env a ns ts) ds in
     mkFix ((is, i), (ns, ts', ds'))
  | CoFix (i, (ns, ts, ds)) ->
     let ts' = Array.map (map_rec env a) ts in
     let ds' = Array.map (map_rec_env_fix map_rec d env a ns ts) ds in
     mkCoFix (i, (ns, ts', ds'))
  | Proj (p, c) ->
     let c' = map_rec env a c in
     mkProj (p, c')
  | _ ->
     f env a trm

(*
 * Map a function over a term, when the environment doesn't matter
 * Update the argument of type 'a using the a supplied update function
 * Return a new term
 *)
let map_term f d (a : 'a) (trm : types) : types =
  map_term_env (fun _ a t -> f a t) d empty_env a trm

(* --- Names --- *)

(* Look up the name referenced by a term and append a suffix to it. *)
let suffix_term_name term suffix =
  let base = Nametab.basename_of_global (Globnames.global_of_constr term) in
  Nameops.add_suffix base (Names.Id.to_string suffix)

(* Add a suffix to a name identifier *)
let with_suffix id suffix =
  let prefix = Id.to_string id in
  Id.of_string (String.concat "_" [prefix; suffix])

(* Turn a name into an optional identifier *)
let ident_of_name = function
  | Name id -> Some id
  | Anonymous -> None

(* Turn an identifier into an external (i.e., surface-level) reference *)
let reference_of_ident id =
  Libnames.Ident id |> CAst.make

(* Turn a name into an optional external (i.e., surface-level) reference *)
let reference_of_name =
  ident_of_name %> Option.map reference_of_ident

(* Convert an external reference into a qualid *)
let qualid_of_reference =
  Libnames.qualid_of_reference %> CAst.with_val identity

(* Convert a term into a global reference with universes (or raise Not_found) *)
let pglobal_of_constr term =
  match Constr.kind term with
  | Const (const, univs) -> ConstRef const, univs
  | Ind (ind, univs) -> IndRef ind, univs
  | Construct (cons, univs) -> ConstructRef cons, univs
  | Var id -> VarRef id, Univ.Instance.empty
  | _ -> raise Not_found

(* Convert a global reference with universes into a term *)
let constr_of_pglobal (glob, univs) =
  match glob with
  | ConstRef const -> mkConstU (const, univs)
  | IndRef ind -> mkIndU (ind, univs)
  | ConstructRef cons -> mkConstructU (cons, univs)
  | VarRef id -> mkVar id

type global_substitution = global_reference Globmap.t

(* Substitute global references throughout a term *)
let subst_globals subst term =
  let rec aux term =
    try
      pglobal_of_constr term |>
      map_puniverses (flip Globmap.find subst) |>
      constr_of_pglobal
    with Not_found ->
      Constr.map aux term
  in
  aux term