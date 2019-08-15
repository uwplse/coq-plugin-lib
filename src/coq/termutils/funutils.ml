(*
 * Utilities for functions (lambda) and function types (prod)
 *)

open EConstr

(* --- Constructing functions and function types --- *)

(* Recursively turn a product into a function *)
let rec prod_to_lambda sigma trm =
  match kind sigma trm with
  | Prod (n, t, b) ->
     mkLambda (n, t, prod_to_lambda sigma b)
  | _ ->
     trm

(* Recursively turn a function into a product *)
let rec lambda_to_prod sigma trm =
  match kind sigma trm with
  | Lambda (n, t, b) ->
     mkProd (n, t, lambda_to_prod sigma b)
  | _ ->
     trm

(* --- Basic questions about functions or function types --- *)

(*
 * Get the arity of a function or function type
 *)
let rec arity sigma p =
  match kind sigma p with
  | Lambda (_, _, b) ->
     1 + arity sigma b
  | Prod (_, _, b) ->
     1 + arity sigma b
  | _ ->
     0
