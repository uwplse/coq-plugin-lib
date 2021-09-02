(*
 * Utilities for names, references, and identifiers
 *)

open Constr
open Names
open Util

(* Look up the name referenced by a term and append a suffix to it. *)
let suffix_term_name term suffix =
  let base = Nametab.basename_of_global (Globnames.global_of_constr term) in
  Nameops.add_suffix base (Names.Id.to_string suffix)

(* Add a suffix to a name identifier *)
let with_suffix id suffix =
  let prefix = Id.to_string id in
  Id.of_string (String.concat "_" [prefix; suffix])

(* Turn a name into an optional identifier *)
let ident_of_name = function
  | Name id -> Some id
  | Anonymous -> None
            
(* Unwrap a Name.t expecting an Id.t. Fails if anonymous. *)
let expect_name = function
  | Name n -> n
  | Anonymous ->
     failwith "Unexpected Anonymous Name.t."

