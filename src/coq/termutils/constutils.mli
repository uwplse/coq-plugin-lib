(*
 * Utilities for constants
 *)

open Constr
open Environ
open Names

(* --- Constructing constants --- *)

(*
 * Define a constant from an ID in the current path
 *)
val make_constant: Id.t -> types

(* --- Opening constants --- *)

(*
 * Safely extract the body of a constant, instantiating any universe variables.
 * If needed, an evar_map should be constructed from the updated environment 
 * with Evd.from_env (TODO maybe should just take evar_map and return updated).
 *
 * Raises a Match_failure if the constant does not exist.
 *)
val open_constant : env -> Constant.t -> env * constr
