(*
 * Utilities for defining terms
 *)

open Constr
open Names
open Evd
open Environ
open Constrexpr
open Globnames

(* --- Defining Coq terms --- *)

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

(* --- Converting between representations --- *)

(*
 * Coq has many ways of representing terms. These functions convert
 * between different representations, using extra information (environments,
 * evar maps, and so on) as necessary
 *
 * The internal representation is constr or types (types aliases to constr).
 * The type constr stands for "construction," not constructor.
 *
 * The external representation is constr_expr.
 *
 * References to definitions are represented by the global_reference type.
 * Note that this is what the define_term and define_canonical functions
 * above return. The type global_reference Univ.puniverses additionally 
 * stores universes.
 *)

(*
 * Intern a term
 *)
val intern : env -> evar_map -> constr_expr -> evar_map * types

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

(*
 * Safely instantiate a global reference, updating the evar map
 *)
val new_global : evar_map -> global_reference -> evar_map * constr
