(*
 * Coq term and environment management
 *)

open Context
open Environ
open Constr
open Evd
open Constrexpr
open Names
open Declarations
open Globnames
open Decl_kinds

module Globmap = Globnames.Refmap
module Globset = Globnames.Refset

module CRD = Context.Rel.Declaration

(* --- Auxiliary types --- *)
               
type closure = env * (types list)

(* --- Constants --- *)

val sigT : types
val existT : types
val sigT_rect : types
val projT1 : types
val projT2 : types
val eq : types
val eq_refl : types
val eq_ind_r : types
val eq_ind : types
val eq_rec_r : types
val eq_rec : types
val eq_sym : types

(* --- Questions about constants --- *)

(*
 * Determine if a term applies an identity term
 * For efficiency, don't consider convertible terms
 *)
val applies_identity : types -> bool

(*
 * Check if a term is a rewrite via eq_ind or eq_ind_r
 * For efficiency, don't consider convertible terms
 *)
val is_rewrite : types -> bool

(* --- Convenient applications of constants --- *)

(*
 * Get the Coq identity function instantiated at a given type
 *)
val identity_term : env -> types -> types
                            
(* --- Representations --- *)

(** Construct the external expression for a definition. *)
val expr_of_global : global_reference -> constr_expr

(*
 * Intern a term (for now, ignore the resulting evar_map)
 *)
val intern : env -> evar_map -> constr_expr -> types

(*
 * Extern a term
 *)
val extern : env -> evar_map -> types -> constr_expr

(*
 * Yves Bertot's edeclare, with extra optional type-checking call (see comment)
 *)
val edeclare :
  Id.t ->
  (locality * polymorphic * definition_object_kind) ->
  opaque:'a ->
  evar_map ->
  UState.universe_decl ->
  EConstr.constr ->
  EConstr.t option ->
  Impargs.manual_implicits ->
  global_reference Lemmas.declaration_hook ->
  bool ->
  global_reference

(*
 * Define a new Coq term
 * Refresh universes if the bool is true, otherwise don't
 * (Refreshing universes is REALLY costly)
 *)
val define_term : ?typ:types -> Id.t -> evar_map -> types -> bool -> global_reference

(*
 * Safely extract the body of a constant, instantiating any universe variables.
 * If needed, an evar_map should be constructed from the updated environment with
 * Evd.from_env.
 *
 * Raises a Match_failure if the constant does not exist.
 *)
val open_constant : env -> Constant.t -> env * constr

(* --- Constructing terms --- *)

(*
 * mkApp with a list (instead of an array) of arguments
 *)
val mkAppl : (types * types list) -> types

(*
 * Define a constant from an ID in the current path
 *)
val make_constant: Id.t -> types
                                                 
(*
 * Switch between products and lambdas, without changing anything else
 *)
val prod_to_lambda : types -> types
val lambda_to_prod : types -> types
                        
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
 * Convert between a term and an eq_app
 *)
val apply_eq : eq_app -> types
val dest_eq : types -> eq_app

(*
 * An application of eq_sym
 *)
type eq_sym_app =
  {
    eq_typ : eq_app;
    eq_proof : types;
  }

(*
 * Convert between a term and an eq_sym_app
 *)
val apply_eq_sym : eq_sym_app -> types
val dest_eq_sym : types -> eq_sym_app

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
 * Convert between a term and an eq_app
 *)
val apply_eq_ind : eq_ind_app -> types
val dest_eq_ind : types -> eq_ind_app
                                
(*
 * An application of eq_refl
 *)
type eq_refl_app =
  {
    typ : types;
    trm : types;
  }

(*
 * Convert between a term and an eq_refl
 *)
val apply_eq_refl : eq_refl_app -> types
val dest_eq_refl : types -> eq_refl_app
                                
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
 * Convert between a term and an existT_app
 *)
val pack_existT : existT_app -> types
val dest_existT : types -> existT_app

(*
 * An application of sigT
 *)
type sigT_app =
  {
    index_type : types;
    packer : types;
  }

(*
 * Convert between a term and a sigT_app
 *)
val pack_sigT : sigT_app -> types
val dest_sigT : types -> sigT_app

(*
 * Build the eta-expansion of a term known to have a sigma type.
 *)
val eta_sigT : constr -> types -> constr

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
 * Convert between a term and a sigT_elim
 *)
val elim_sigT : sigT_elim -> types
val dest_sigT_elim : types -> sigT_elim

(*
 * Left projection of a sigma type given a sigma type and term of that type
 *)
val project_index : sigT_app -> types -> types

(*
 * Right projection of a sigma type given a sigma type and term of that type
 *)
val project_value : sigT_app -> types -> types

(*
 * Both projections in an (index, value) tuple
 *)
val projections : sigT_app -> types -> (types * types)

(* --- Inductive types and their eliminators --- *)

(*
 * Fail if the inductive type is mutually inductive or coinductive
 *)
val check_inductive_supported : mutual_inductive_body -> unit

(*
 * Get the number of constructors for an inductive type
 *)
val num_constrs : mutual_inductive_body -> int

(*
 * Get an inductive type from an eliminator, if possible
 *)
val inductive_of_elim : env -> pconstant -> mutual_inductive option

(* --- Environments --- *)

(* Look up all indexes from a list in an environment *)
val lookup_rels : int list -> env -> Rel.Declaration.t list

(* Return a list of all indexes in an environment, starting with 1 *)
val all_rel_indexes : env -> int list

(* Return a list of all bindings in an environment, starting with the closest *)
val lookup_all_rels : env -> Rel.Declaration.t list

(*
 * Push to an environment
 *)
val push_local : (name * types) -> env -> env
val push_let_in : (name * types * types) -> env -> env

(* Is the rel declaration a local assumption? *)
val is_rel_assum : ('constr, 'types) Rel.Declaration.pt -> bool

(* Is the rel declaration a local definition? *)
val is_rel_defin : ('constr, 'types) Rel.Declaration.pt -> bool

(*
 * Construct a rel declaration
 *)
val rel_assum : Name.t * 'types -> ('constr, 'types) Rel.Declaration.pt
val rel_defin : Name.t * 'constr * 'types -> ('constr, 'types) Rel.Declaration.pt

(*
 * Project a component of a rel declaration
 *)
val rel_name : ('constr, 'types) Rel.Declaration.pt -> Name.t
val rel_value : ('constr, 'types) Rel.Declaration.pt -> 'constr option
val rel_type : ('constr, 'types) Rel.Declaration.pt -> 'types

(*
 * Map over a rel context with environment kept in synch
 *)
val map_rel_context : env -> (env -> Rel.Declaration.t -> 'a) -> Rel.t -> 'a list

(*
 * Bind all local declarations in the relative context onto the body term as
 * products, substituting away (i.e., zeta-reducing) any local definitions.
 *)
val smash_prod_assum : Rel.t -> types -> types

(*
 * Bind all local declarations in the relative context onto the body term as
 * lambdas, substituting away (i.e., zeta-reducing) any local definitions.
 *)
val smash_lam_assum : Rel.t -> constr -> constr

(*
 * Decompose the first n product bindings, zeta-reducing let bindings to reveal
 * further product bindings when necessary.
 *)
val decompose_prod_n_zeta : int -> types -> Rel.t * types

(*
 * Decompose the first n lambda bindings, zeta-reducing let bindings to reveal
 * further lambda bindings when necessary.
 *)
val decompose_lam_n_zeta : int -> constr -> Rel.t * constr

(*
 * Lookup from an environment
 *)
val lookup_pop : int -> env -> (env * CRD.t list)
val lookup_definition : env -> types -> types
val unwrap_definition : env -> types -> types

(*
 * Get bindings to push to an environment
 *)
val bindings_for_inductive :
  env -> mutual_inductive_body -> one_inductive_body array -> CRD.t list
val bindings_for_fix : name array -> types array -> CRD.t list

(*
 * Reconstruct local bindings around a term
 *)
val recompose_prod_assum : Rel.t -> types -> types
val recompose_lam_assum : Rel.t -> types -> types

(* --- Basic questions about terms --- *)

(* Is the first term equal to a "head" (application prefix) of the second?
 * The notion of term equality is syntactic, by default modulo alpha, casts,
 * application grouping, and universes. The result of this function is an
 * informative boolean: an optional array, with None meaning false and Some
 * meaning true and giving the trailing arguments.
 *
 * This function is similar to is_or_applies, except for term equality and the
 * informative boolean result.
 *)
val eq_constr_head : ?eq_constr:(constr -> constr -> bool) -> constr -> constr -> constr array option

(* --- Convertibility, reduction, and types --- *)

(*
 * Type-checking
 *
 * Current implementation may cause universe leaks, which will just cause
 * conservative failure of the plugin
 *)
val infer_type : env -> evar_map -> types -> types

(* Check whether a term has a given type *)
val has_type : env -> evar_map -> types -> types -> bool

(* Safely infer the WHNF type of a term, updating the evar map. *)
val e_infer_type : env -> evar_map ref -> constr -> constr

(* Safely infer the sort of a term, updating the evar map. *)
val e_infer_sort : env -> evar_map ref -> constr -> Sorts.family

(* Safely instantiate a global reference, updating the evar map. *)
val e_new_global : evar_map ref -> global_reference -> constr

(* Convertibility, ignoring universe inconsistency for now *)
val convertible : env -> evar_map -> types -> types -> bool
                                                         
(*
 * Checks whether the conclusions of two dependent types are convertible,
 * modulo the assumption that every argument we encounter is equal when
 * the types of those arguments are convertible. Expect exactly the same
 * number of arguments in the same order.
 *
 * For example, the following are true:
 *    concls_convertible empty Evd.empty (forall (a : nat), a) (forall (a : nat) b, a)
 *    concls_convertible empty Evd.empty (forall (a : nat), a) (forall (a : nat), a)
 *    concls_convertible empty Evd.empty (forall (a : nat), True) (forall (b : bin), True)
 *
 * The following are false:
 *    concls_convertible empty Evd.empty (forall a, True) False
 *    concls_convertible empty Evd.empty (forall a, True) True
 *    concls_convertible empty Evd.empty (forall (a : nat), a) (forall (a : bin), a)
 *    concls_convertible empty Evd.empty (forall a b, a) (forall a b, b)
 *
 * Assumes types are locally closed.
 *)
val concls_convertible : env -> evar_map -> types -> types -> bool

(*
 * Reduction
 *)
val reduce_term : env -> types -> types (* betaiotazeta *)
val delta : env -> types -> types (* delta *)
val reduce_nf : env -> types ->  types (* nf_all *)
val reduce_type : env -> evar_map -> types -> types (* betaiotazeta on types *)
val chain_reduce : (* sequencing *)
  (env -> types -> types) ->
  (env -> types -> types) ->
  env ->
  types ->
  types

(*
 * Apply a function on a type instead of on the term
 *)
val on_type : (types -> 'a) -> env -> evar_map -> types -> 'a

(* 
 * Checks whether the types of two terms are convertible
 *)
val types_convertible : env -> evar_map -> types -> types -> bool

(* --- Names --- *)

(* Look up the name referenced by a term and append a suffix to it. *)
val suffix_term_name : constr -> Names.Id.t -> Names.Id.t

(* Convert an external reference into a qualid *)
val qualid_of_reference : Libnames.reference -> Libnames.qualid

(* Convert a term into a global reference with universes (or raise Not_found) *)
val pglobal_of_constr : constr -> global_reference Univ.puniverses

(* Convert a global reference with universes into a term *)
val constr_of_pglobal : global_reference Univ.puniverses -> constr

type global_substitution = global_reference Globmap.t

(* Substitute global references throughout a term *)
val subst_globals : global_substitution -> constr -> constr

(* --- Modules --- *)

(* Type-sensitive transformation of terms *)
type constr_transformer = env -> evar_map ref -> constr -> constr

(*
 * Declare a new constant under the given name with the transformed term and
 * type from the given constant.
 *
 * NOTE: Global side effects.
 *)
val transform_constant : Id.t -> constr_transformer -> constant_body -> Constant.t

(*
 * Declare a new inductive family under the given name with the transformed type
 * arity and constructor types from the given inductive definition. Names for
 * the constructors remain the same.
 *
 * NOTE: Global side effects.
 *)
val transform_inductive : Id.t -> constr_transformer -> Inductive.mind_specif -> inductive

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
val transform_module_structure : ?init:(unit -> global_substitution) -> Id.t -> constr_transformer -> module_body -> ModPath.t

(* --- Application and arguments --- *)

(*
 * Get a list of all arguments of a type unfolded at the head
 * Return empty if it's not an application
 *)
val unfold_args : types -> types list

(*
 * Get the very last argument of an application
 *)
val last_arg : types -> types

(*
 * Get the very first function of an application
 *)
val first_fun : types -> types

(*
 * Fully unfold arguments, then get the argument at a given position
 *)
val get_arg : int -> types -> types
