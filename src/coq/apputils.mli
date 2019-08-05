(*
 * Utilities for applications of terms
 *)

open Constr

(* --- Constructing applications --- *)

(*
 * mkApp with a list (instead of an array) of arguments
 *)
val mkAppl : (types * types list) -> types

(* --- Deconstructing applications --- *)

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

(* --- Questions about application --- *)

(*
 * Check whether a term (second argument) applies a function (first argument).
 * Don't consider terms convertible to the function.
 *)
val applies : types -> types -> bool

(*
 * Check whether a term either is exactly a function or applies it.
 *)
val is_or_applies : types -> types -> bool

(* 
 * Is the first term equal to a "head" (application prefix) of the second?
 * The notion of term equality is syntactic, by default modulo alpha, casts,
 * application grouping, and universes. The result of this function is an
 * informative boolean: an optional array, with None meaning false and Some
 * meaning true and giving the trailing arguments.
 *
 * This function is similar to is_or_applies, except for term equality and the
 * informative boolean result.
 *)
val eq_constr_head :
  ?eq_constr:(constr -> constr -> bool) ->
  constr ->
  constr ->
  constr array option
