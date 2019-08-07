(*
 * Utilities for identity types
 *)

open Constr
open Environ
open Evd
       
(* --- Constants --- *)

(* Identity over Prop *)
val id_prop : types

(* Identity over Type *)
val id_typ : types
       
(* --- Representations --- *)

(*
 * Apply identity (id_prop or id_typ) to the input, as appropriate
 *)
val identity_term : env -> evar_map -> types -> evar_map * types

(* --- Questions about constants --- *)

(*
 * Determine if a term (exactly) applies an identity term
 * For efficiency, don't consider convertible terms
 *)
val applies_identity : types -> bool
