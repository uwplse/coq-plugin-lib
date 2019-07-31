(*
 * Utilities for answering questions about how two terms relate to each other
 *)

open Constr
open Environ
open Evd

(* --- Plural versions of questions about terms from Coqterms --- *)

(*
 * Plural version of Coqterms.applies.
 * Check whether two terms (the second and third arguments) apply
 * a given function (first argument). Don't consider terms convertible
 * to the function.
 *)
val apply : types -> types -> types -> bool

(*
 * Plural version of Coqterms.is_or_applies.
 * Check whether two terms (the second and third arguments) either are exactly 
 * a function or apply it.
 *)
val are_or_apply : types -> types -> types -> bool

