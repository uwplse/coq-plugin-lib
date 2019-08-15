(*
 * Utilities for names, references, and identifiers
 *)

open EConstr
open Names
open Util

(* Look up the name referenced by a term and append a suffix to it. *)
let suffix_term_name sigma term suffix =
  let base = Nametab.basename_of_global (Globnames.global_of_constr (EConstr.to_constr sigma term)) in
  Nameops.add_suffix base (Names.Id.to_string suffix)

(* Add a suffix to a name identifier *)
let with_suffix id suffix =
  let prefix = Id.to_string id in
  Id.of_string (String.concat "_" [prefix; suffix])

(* Turn a name into an optional identifier *)
let ident_of_name = function
  | Name id -> Some id
  | Anonymous -> None

(* Turn an identifier into an external (i.e., surface-level) reference *)
let reference_of_ident id =
  Libnames.Ident id |> CAst.make

(* Turn a name into an optional external (i.e., surface-level) reference *)
let reference_of_name =
  ident_of_name %> Option.map reference_of_ident

(* Convert an external reference into a qualid *)
let qualid_of_reference =
  Libnames.qualid_of_reference %> CAst.with_val identity
