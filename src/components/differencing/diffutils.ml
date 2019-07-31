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
