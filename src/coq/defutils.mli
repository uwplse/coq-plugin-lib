(*
 * Utilities for defining terms
 *)

open Constr
open Names
open Evd

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
