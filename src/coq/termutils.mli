(*
 * Coq term and environment management
 *)

open Context
open Environ
open Constr
open Evd
open Constrexpr
open Names
open Globnames
open Decl_kinds

module Globmap = Globnames.Refmap
module Globset = Globnames.Refset
                            
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
 * global_reference Univ.puniverses additionally stores universes.
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
 * Construct the external expression for a definition.
 *)
val expr_of_global : global_reference -> constr_expr

(*
 * Convert a term into a global reference with universes (or raise Not_found) 
 *)
val pglobal_of_constr : constr -> global_reference Univ.puniverses

(* 
 * Convert a global reference with universes into a term
 *)
val constr_of_pglobal : global_reference Univ.puniverses -> constr

(* --- Environments (TODO rename/decouple/move more) --- *)

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

type global_substitution = global_reference Globmap.t

(* Substitute global references throughout a term *)
val subst_globals : global_substitution -> constr -> constr
