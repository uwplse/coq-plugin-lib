(*
 * Utilities for answering questions about how two terms relate to each other
 * TODO make a unified diffutils that exports all of these?
 *)

open Constr
open Coqterms
open Utilities
open Environ
open Evd

(* --- Plural versions of questions about terms from Coqterms --- *)

let are_or_apply (trm : types) = and_p (is_or_applies trm)
let apply (trm : types) = and_p (applies trm)

(* --- Convertibility --- *)

(*
 * TODO very bad evar_map practice here will change soon
 *)

(* Check whether two terms are convertible, ignoring universe inconsistency *)
let conv_ignoring_univ_inconsistency env evm (trm1 : types) (trm2 : types) : bool =
  match map_tuple kind (trm1, trm2) with
  | (Sort (Type u1), Sort (Type u2)) ->
     (* PUMPKIN assumes universe consistency for now *)
     true
  | _ ->
     let etrm1 = EConstr.of_constr trm1 in
     let etrm2 = EConstr.of_constr trm2 in
     try
       Reductionops.is_conv env evm etrm1 etrm2
     with _ ->
       false

(* Checks whether two terms are convertible in env with no evars *)
let convertible (env : env) (evd : evar_map) (trm1 : types) (trm2 : types) : bool =
  conv_ignoring_univ_inconsistency env Evd.empty trm1 trm2

(*
 * Checks whether the conclusions of two dependent types are convertible,
 * modulo the assumption that every argument we encounter is equal when
 * the types of those arguments are convertible. Expect exactly the same
 * number of arguments in the same order.
 *)
let rec concls_convertible (env : env) (evd : evar_map) (typ1 : types) (typ2 : types) : bool =
  match (kind typ1, kind typ2) with
  | (Prod (n1, t1, b1), Prod (n2, t2, b2)) ->
     if convertible env evd t1 t2 then
       concls_convertible (push_rel CRD.(LocalAssum(n1, t1)) env) evd b1 b2
     else
       false
  | _ ->
     convertible env evd typ1 typ2
