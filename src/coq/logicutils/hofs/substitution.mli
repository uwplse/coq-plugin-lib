(* Substitution auxiliary functions *)

open Environ
open Constr
open Evd

(* TODO clean up so retrieval is easier *)

(*
 * Note: These substitution functions assume that all of the necessary
 * constraints for both the source and destination terms are already in the 
 * provided evar_map. Thus, they do not update the evar_map as they recurse.
 * If the destination type refers to evars or universes that are not in
 * the evar_map, then these functions may produce terms that are not
 * well-typed. In general, these functions may produce terms that are not
 * well-typed even regardless of evar_map problems; it is up to you
 * to check the returned terms later on.
 *)
type ('a, 'b) substitution = env -> evar_map -> 'a -> types -> evar_map * 'b
type 'a comb_substitution = ('a, types list) substitution
type 'a type_substitution = ('a, types) substitution

(*
 * In an environment, substitute all subterms of a term that are
 * convertible to a source term with a destination term.
 *
 * This checks convertibility before recursing, and so will replace at
 * the highest level possible.
 *)
val all_conv_substs : (types * types) type_substitution

(*
 * In an environment, substitute all subterms of a term that have
 * a convertible type to the type of a source term with a
 * destination term.
 *
 * This checks convertibility before recursing, and so will replace at
 * the highest level possible.
 *)
val all_typ_substs : (types * types) type_substitution

(*
 * all_substs with eq_constr and the empty environment
 *)
val all_eq_substs : (types * types) -> types -> types

(*
 * In an environment, substitute all subterms of a term that apply a
 * constructor with the first argument with the same type as the constructor.
 * This effectively "undoes" the constructor.
 *
 * It's currently not smart enough to understand what to do when the
 * constructor has multiple arguments of the same type as the type itself,
 * like in tree-like inductive types. It's always going to try the left
 * case in a tree for now.
 *
 * This checks convertibility before recursing, and so will replace at
 * the highest level possible.
 *)
val all_constr_substs : types type_substitution

(*
 * In an environment, return all combinations of substitutions of
 * subterms of a term that are convertible with a source term
 * with a destination term.
 *)
val all_conv_substs_combs : (types * types) comb_substitution

(*
 * In an environment, return all combinations of substitutions of
 * subterms of a term that have a type that is convertible with
 * the type of a source term  with a destination term.
 *)
val all_typ_substs_combs : (types * types) comb_substitution

(* --- Substituting global references (TODO unify w/ above after cleaning types) --- *)

type global_substitution = Globnames.global_reference Names.GlobRef.Map.t

(* Substitute global references throughout a term *)
val subst_globals : global_substitution -> constr -> constr
