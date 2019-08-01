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

(* Turn an identifier into an external (i.e., surface-level) reference *)
val reference_of_ident : Id.t -> Libnames.reference

(* Turn a name into an optional external (i.e., surface-level) reference *)
val reference_of_name : Name.t -> Libnames.reference option

(* Convert an external reference into a qualid *)
val qualid_of_reference : Libnames.reference -> Libnames.qualid
