(*
 * Utilities for differencing types
 *)

open Constr
open Environ
open Evd

(* --- Convertibility --- *)

(* 
 * Checks whether the types of two terms are convertible
 *)
val types_convertible : env -> evar_map -> types -> types -> bool
