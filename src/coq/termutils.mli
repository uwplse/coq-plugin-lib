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
                            
(* --- Representations --- *)

(*
 * Coq has many ways of representing terms. These functions convert
 * between different representations, using extra information (environments,
 * evar maps, and so on) as necessary
 *
 * The internal representation is constr or types (types aliases to constr).
 * Constr stands for "construction," not constructor.
 *
 * The external representation is constr_expr.
 *
 * References to definitions are represented by the global_reference type.
 *)

(*
 * Intern a term (for now, ignore the resulting evar_map)
 *)
val intern : env -> evar_map -> constr_expr -> types

(*
 * Extern a term
 *)
val extern : env -> evar_map -> types -> constr_expr

(*
 * Define a new Coq term
 * Refresh universes if the bool is true, otherwise don't
 * (Refreshing universes is REALLY costly)
 *)
val define_term :
  ?typ:types -> Id.t -> evar_map -> types -> bool -> global_reference

(*
 * Like define_term, but for a canonical structure
 *)
val define_canonical :
  ?typ:types -> Id.t -> evar_map -> types -> bool -> global_reference

(* 
 * Construct the external expression for a definition.
 *)
val expr_of_global : global_reference -> constr_expr
                                                       
(*
 * Safely extract the body of a constant, instantiating any universe variables.
 * If needed, an evar_map should be constructed from the updated environment with
 * Evd.from_env.
 *
 * Raises a Match_failure if the constant does not exist.
 * TODO move
 *)
val open_constant : env -> Constant.t -> env * constr

(* --- Constructing terms --- *)

(*
 * Define a constant from an ID in the current path
 *)
val make_constant: Id.t -> types
                                                 
(*
 * Switch between products and lambdas, without changing anything else
 *)
val prod_to_lambda : types -> types
val lambda_to_prod : types -> types

(* --- Environments (TODO rename/decouple/move more) --- *)

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

(* Is the named declaration an assumption? *)
val is_named_assum : ('constr, 'types) Named.Declaration.pt -> bool

(* Is the named declaration a definition? *)
val is_named_defin : ('constr, 'types) Named.Declaration.pt -> bool

(*
 * Construct a named declaration
 *)
val named_assum : Id.t * 'types -> ('constr, 'types) Named.Declaration.pt
val named_defin : Id.t * 'constr * 'types -> ('constr, 'types) Named.Declaration.pt

(*
 * Project a component of a named declaration
 *)
val named_ident : ('constr, 'types) Named.Declaration.pt -> Id.t
val named_value : ('constr, 'types) Named.Declaration.pt -> 'constr option
val named_type : ('constr, 'types) Named.Declaration.pt -> 'types

(*
 * Map over a named context with environment kept in synch
 *)
val map_named_context : env -> (env -> Named.Declaration.t -> 'a) -> Named.t -> 'a list                                                                 

(*
 * Get bindings to push to an environment
 *)
val bindings_for_fix : name array -> types array -> CRD.t list

(*
 * Append two contexts (inner first, outer second), shifting internal indices.
 *
 * The input contexts are assumed to share the same environment, such that any
 * external indices inside the now-inner context must be shifted to pass over
 * the now-outer context.
 *)
val context_app : Rel.t -> Rel.t -> Rel.t
                                                          
(*
 * Reconstruct local bindings around a term
 *)
val recompose_prod_assum : Rel.t -> types -> types
val recompose_lam_assum : Rel.t -> types -> types

(* --- Basic questions about terms --- *)

(*
 * Get the arity of a function or function type
 *)
val arity : types -> int
                                              
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

(* --- Convertibility and reduction --- *)

(* Safely instantiate a global reference, updating the evar map. (TODO move) *)
val e_new_global : evar_map ref -> global_reference -> constr

(* --- Names (TODO move/rename remaining) --- *)

(* Convert a term into a global reference with universes (or raise Not_found) *)
val pglobal_of_constr : constr -> global_reference Univ.puniverses

(* Convert a global reference with universes into a term *)
val constr_of_pglobal : global_reference Univ.puniverses -> constr

type global_substitution = global_reference Globmap.t

(* Substitute global references throughout a term *)
val subst_globals : global_substitution -> constr -> constr
