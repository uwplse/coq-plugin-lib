(*
 * Utilities for functions (lambda) and function types (prod)
 *)

open Constr

(* --- Constructing functions and function types --- *)

(*
 * It is sometimes useful to define a single OCaml function that operates
 * over functions or over function types, then to simply convert the
 * term to the appropriate representation to use that function. That way,
 * you don't need to redefine the same function multiple times to do the same
 * thing over functions and over function types.
 *
 * These functions let you convert between functions and function types
 * without changing anything else. There is no guarantee that the result
 * will type check, but this can be very useful for development.
 *)
val prod_to_lambda : types -> types
val lambda_to_prod : types -> types
