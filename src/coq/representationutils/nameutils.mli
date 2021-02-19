(*
 * Utilities for names, references, and identifiers
 *)

open Constr
open Names

(* Look up the name referenced by a term and append a suffix to it. *)
val suffix_term_name : constr -> Names.Id.t -> Names.Id.t

(* Add a string suffix to a name identifier *)
val with_suffix : Id.t -> string -> Id.t

(* Turn a name into an optional identifier *)
val ident_of_name : Name.t -> Id.t option

(* Unwrap a Name.t expecting an Id.t. Fails if anonymous. *)
val expect_name : Name.t -> Id.t

