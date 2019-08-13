(*
 * Convertibility
 *
 * These functions update constraints in the evar_map, so the resulting
 * evar_map must be threaded through.
 *)

open Constr
open Environ
open Evd

(* Convertibility under some set of constraints *)
val convertible : env -> evar_map -> types -> types -> evar_map * bool
                                                
(*
 * Checks whether the conclusions of two dependent types are convertible,
 * modulo the assumption that every argument we encounter is equal when
 * the types of those arguments are convertible. Expect exactly the same
 * number of arguments in the same order.
 *
 * For example, the following are true (assuming a reasonable environment env
 * and evar_map sigma):
 *    concls_convertible env sigma (forall (a : nat), a) (forall (a : nat) b, a)
 *    concls_convertible env sigma (forall (a : nat), a) (forall (a : nat), a)
 *    concls_convertible env sigma (forall (a : nat), True) (forall (b : bin), True)
 *
 * The following are false:
 *    concls_convertible env sigma (forall a, True) False
 *    concls_convertible env sigma (forall a, True) True
 *    concls_convertible env sigma (forall (a : nat), a) (forall (a : bin), a)
 *    concls_convertible env sigma (forall a b, a) (forall a b, b)
 *
 * Assumes types are locally closed.
 *)
val concls_convertible : env -> evar_map -> types -> types -> evar_map * bool

(* 
 * Checks whether the types of two terms are convertible
 *)
val types_convertible : env -> evar_map -> types -> types -> evar_map * bool
