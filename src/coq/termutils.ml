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
open Recordops

module Globmap = Globnames.Refmap
module Globset = Globnames.Refset

module CRD = Context.Rel.Declaration (* TODO remove eventually *)

(* --- Auxiliary types --- *)
               
type closure = env * (types list)
                                                                    
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

(* Define a Canonical Structure *)
let define_canonical ?typ (n : Id.t) (evm : evar_map) (trm : types) (refresh : bool) =
  let k = (Global, Flags.is_universe_polymorphism (), CanonicalStructure) in
  let udecl = Univdecls.default_univ_decl in
  let hook = Lemmas.mk_hook (fun _ x -> declare_canonical_structure x; x) in
  let etrm = EConstr.of_constr trm in
  let etyp = Option.map EConstr.of_constr typ in
  edeclare n k ~opaque:false evm udecl etrm etyp [] hook refresh

(* Safely extract the body of a constant, instantiating any universe variables. TODO move *)
let open_constant env const =
  let (Some (term, auctx)) = Global.body_of_constant const in
  let uctx = Universes.fresh_instance_from_context auctx |> Univ.UContext.make in
  let term = Vars.subst_instance_constr (Univ.UContext.instance uctx) term in
  let env = Environ.push_context uctx env in
  env, term

(* --- Constructing terms --- *)

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

(* --- Convertibility and reduction --- *)

(* Safely instantiate a global reference, with proper universe handling (TODO move) *)
let e_new_global evm gref =
  Evarutil.e_new_global evm gref |> EConstr.to_constr !evm

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

(* Is the first term equal to a "head" (application prefix) of the second?
 * The notion of term equality is syntactic (i.e., no environment) and defaults
 * to syntactic equality modulo alpha, casts, grouping, and universes. The
 * result of this function is an informative boolean: an optional array, with
 * None meaning false and Some meaning true and giving the trailing arguments.
 *
 * This function is similar to is_or_applies, except for term equality and the
 * informative boolean result.
 *
 * TODO move, maybe don't expose
 *)
let eq_constr_head ?(eq_constr=eq_constr_nounivs) term term' =
  let head, args = decompose_app term in
  let head', args' = decompose_app term' in
  if eq_constr head head' && List.prefix_of eq_constr args args' then
    Some (List.skipn (List.length args) args' |> Array.of_list)
  else
    None

(* --- Names --- *)

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
