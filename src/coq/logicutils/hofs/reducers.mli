(* Strategies for reducing terms *)

open Environ
open Evd
open Constr

type reducer = env -> evar_map -> types -> types
type e_reducer = env -> evar_map -> types -> evar_map * types

(* --- Top-level --- *)

(*
 * Default reducer (currently betaiotazeta) at term and type level
 *)
val reduce_term : reducer
val reduce_type : e_reducer

(*
 * Other reducers
 *)
val delta : reducer
val whd : reducer
val reduce_nf : reducer

(* --- Custom reducers --- *)

(*
 * Reduce all using the reducer
 *)
val reduce_all : reducer -> env -> evar_map -> types list -> types list
                    
(*
 * Do not reduce
 *)
val do_not_reduce : reducer

(*
 * Remove all applications of the identity function
 *)
val remove_identities : reducer

(*
 * Remove unused hypotheses
 *)
val remove_unused_hypos : reducer

(*
 * Remove all applications of the identity function, then default reduce
 *)
val reduce_remove_identities : reducer

(*
 * Default reduce and also unfold definitions (delta-reduce, nf)
 *)
val reduce_unfold : reducer

(*
 * Default reduce and also unfold definitions (delta-reduce, whd)
 *)
val reduce_unfold_whd : reducer

(*
 * Weak-head reduce a term if it is a let-in (conditional betaiotazeta, whd)
 *)
val reduce_whd_if_let_in : reducer

(* --- Combinators and converters --- *)

(*
 * Reduce with the first reducer, then with the second reducer
 *)
val chain_reduce : reducer -> reducer -> reducer

(*
 * Try to reduce, but let failure be OK
 *)
val try_reduce : reducer -> reducer

(*
 * Reduce the body of a term using the supplied reducer if
 * the predicate p is true on the body. If the term is a function,
 * then this recurses into the body and checks the condition, and so on.
 * It reduces as soon as the condition holds.
 *)
val reduce_body_if : (env -> evar_map -> types -> bool) -> reducer -> reducer

(*
 * Infer the type, then reduce using supplied reducer
 * Update the evar_map appropriately
 *)
val reduce_type_using : reducer -> e_reducer
