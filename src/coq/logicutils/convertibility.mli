(*
 * Convertibility
 *)

open Constr
open Environ
open Evd

(* Convertibility, ignoring universe inconsistency for now *)
val convertible : env -> evar_map -> types -> types -> bool
                                                
(*
 * TODO update this comment when good evar_map practice is implemented
 *
 * Checks whether the conclusions of two dependent types are convertible,
 * modulo the assumption that every argument we encounter is equal when
 * the types of those arguments are convertible. Expect exactly the same
 * number of arguments in the same order.
 *
 * For example, the following are true:
 *    concls_convertible empty Evd.empty (forall (a : nat), a) (forall (a : nat) b, a)
 *    concls_convertible empty Evd.empty (forall (a : nat), a) (forall (a : nat), a)
 *    concls_convertible empty Evd.empty (forall (a : nat), True) (forall (b : bin), True)
 *
 * The following are false:
 *    concls_convertible empty Evd.empty (forall a, True) False
 *    concls_convertible empty Evd.empty (forall a, True) True
 *    concls_convertible empty Evd.empty (forall (a : nat), a) (forall (a : bin), a)
 *    concls_convertible empty Evd.empty (forall a b, a) (forall a b, b)
 *
 * Assumes types are locally closed.
 *)
val concls_convertible : env -> evar_map -> types -> types -> bool

(* 
 * Checks whether the types of two terms are convertible
 *)
val types_convertible : env -> evar_map -> types -> types -> bool
