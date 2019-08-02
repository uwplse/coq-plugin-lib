(*
 * Utilities for functions (lambda) and function types (prod)
 *)

open Constr

(* --- Constructing functions and function types --- *)

(* Recursively turn a product into a function *)
let rec prod_to_lambda trm =
  match kind trm with
  | Prod (n, t, b) ->
     mkLambda (n, t, prod_to_lambda b)
  | _ ->
     trm

(* Recursively turn a function into a product *)
let rec lambda_to_prod trm =
  match kind trm with
  | Lambda (n, t, b) ->
     mkProd (n, t, lambda_to_prod b)
  | _ ->
     trm

(* --- Basic questions about functions or function types --- *)

(*
 * Get the arity of a function or function type
 *)
let rec arity p =
  match kind p with
  | Lambda (_, _, b) ->
     1 + arity b
  | Prod (_, _, b) ->
     1 + arity b
  | _ ->
     0
