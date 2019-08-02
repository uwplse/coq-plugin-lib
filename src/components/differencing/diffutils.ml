(*
 * Utilities for answering questions about how two terms relate to each other
 *)

open Constr
open Utilities
open Environ
open Evd

(* --- Plural versions of questions about terms --- *)

let are_or_apply (trm : types) = and_p (Apputils.is_or_applies trm)
let apply (trm : types) = and_p (Apputils.applies trm)
